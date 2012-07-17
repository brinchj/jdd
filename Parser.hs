{-# LANGUAGE OverloadedStrings
           , GeneralizedNewtypeDeriving
           , ScopedTypeVariables
 #-}

module Parser
       (
         parseClassFile
       , test
       ) where

import Prelude hiding (take)

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State.Strict as ST
import qualified Control.Monad.Error as E

import Control.Applicative ((<$>), (<*>))

import Text.Parsec.ByteString
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec


import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import Data.Maybe (fromMaybe)
import Data.Char (ord)



data Version = Version { getMajor :: Integer, getMinor :: Integer}
             deriving Show


data Class = Class { classPath :: B.ByteString }
           deriving Show

data Desc = Desc { descName :: B.ByteString
                 , descType :: B.ByteString }
          deriving Show

data Constant = SignedInt Integer
              | Long      Integer
              | Str      { unStr :: B.ByteString }
              | ClassRef { unClassRef :: Class }
              | DescRef  { unDescRef  :: Desc  }
              | Method
                { methodClass :: Class
                , methodDesc  :: Desc }
              deriving Show

data Attribute = Attr
                 { attrName :: B.ByteString
                 , attrBody :: B.ByteString
                 }
               deriving Show

data AttributeBlock = AttrBlock
                      { blockFlags :: Integer
                      , blockName  :: B.ByteString
                      , blockDesc  :: B.ByteString
                      , blockAttrs :: [Attribute]
                      }
                    deriving Show


data ClassFile = ClassFile
                 { classVersion    :: Version
                 , classFlags      :: Integer
                 , classConstants  :: M.Map Integer Constant
                 , classThis       :: Constant
                 , classSuper      :: Constant
                 , classInterfaces :: [AttributeBlock]
                 , classFields     :: [AttributeBlock]
                 , classMethods    :: [AttributeBlock]
                 }
               deriving Show


type ClassParser a = ParsecT B.ByteString () (ST.State ClassFile) a


-- Parse 1-byte, 2-byte, 4-byte and 8-byte unsigned int (big-endian)
u1, u2, u4, u8 :: ClassParser Integer
u1 = fromIntegral . ord <$> anyToken

u2 = do
  a <- u1
  b <- u1
  return $ (a * 2^8) + b

u4 = do
  a <- u2
  b <- u2
  return $ (a * 2^16) + b

u8 = do
  a <- fromIntegral <$> u4
  b <- fromIntegral <$> u4
  return $ (a * 2^32) + b


-- Parse 1-byte, 2-byte, 4-byte, 8-byte signed int (two complement, big-endian)
makeSigned :: Int -> Integer -> Integer
makeSigned bits n | n > 2 ^ (bits - 1) = n + (2 ^ bits)
                  | otherwise          = n

s1, s2, s4, s8 :: ClassParser Integer
s1 = makeSigned 8  <$> u1
s2 = makeSigned 16 <$> u2
s4 = makeSigned 32 <$> u4
s8 = makeSigned 64 <$> u8


-- Parse magic number
magicNumberP :: ClassParser ()
magicNumberP = void $ string "\xCA\xFE\xBA\xBE"

-- Parse version
versionP :: ClassParser Version
versionP = Version <$> u2 <*> u2


-- Parse a string with 2-byte length prefix
string2P :: ClassParser B.ByteString
string2P = B.pack <$> (flip replicateM anyChar =<< fromIntegral <$> u2)

-- Parse constant pool
type ConstantPool = M.Map Integer Constant
constantPoolP :: ClassParser ()
constantPoolP = do
  maxID <- fromIntegral <$> u2
  void $ E.runErrorT $ entriesP $ maxID - 1
  where
    entriesP = flip replicateM_ entryP

    entryP = do
      constant <- convertTag =<< lift u1
      lift $ ST.modify $ \cf ->
        cf { classConstants = insert constant $ classConstants cf }
      where insert c m = M.insert (fromIntegral $ M.size m + 1) c m

    convertTag tag =
      case tag of
        0  -> E.throwError ("EOF" :: String)

        1  -> Str              <$^> string2P
        3  -> SignedInt        <$^> s4
        5  -> Long             <$^> s8
        7  -> ClassRef . Class . unStr <$> get1

        10 -> Method  `get2` unClassRef $ unDescRef
        12 -> DescRef <$> (Desc `get2` unStr  $ unStr)

        _  -> error $ "Unknown constant pool entry-tag: " ++ show tag

      where
        f <$^> g = f <$> lift g

        get1 = do
          idx <- lift u2
          lift $ ST.gets $ fromMaybe (error "invalid id") .
            M.lookup idx . classConstants

        get2 f ma mb = do
          a <- ma <$> get1
          b <- mb <$> get1
          return $ f a b


cpLookup :: Integer -> ClassParser Constant
cpLookup i = ST.gets $ fromMaybe (error $ "invalid id: " ++ show i) .
             M.lookup i . classConstants


many2 :: ClassParser a -> ClassParser [a]
many2 a = do
  n <- fromIntegral <$> u2
  replicateM n a


interfacesP = undefined


blockP :: ClassParser AttributeBlock
blockP = do
  flags <- u2
  Str name <- cpLookup =<< u2
  Str desc <- cpLookup =<< u2
  as <- many2 attributeP
  return $ AttrBlock flags name desc as

addBlocksP :: ([AttributeBlock] -> ClassFile -> ClassFile) -> ClassParser ()
addBlocksP f = do
  blocks <- many2 blockP
  ST.modify $ \cf -> f blocks cf


attributeP :: ClassParser Attribute
attributeP = do
  Str name <- cpLookup =<< u2
  body <- B.pack <$> (flip replicateM anyChar =<< fromIntegral <$> u4)
  return $ Attr name body


fieldsP :: ClassParser ()
fieldsP = addBlocksP $ \fs cf -> cf { classFields = fs }

methodsP :: ClassParser ()
methodsP = addBlocksP $ \ms cf -> cf { classMethods = ms }


classFile :: ClassParser ()
classFile = do
  _ <- magicNumberP
  version <- versionP

  -- Parse and update constant pool
  constantPoolP

  flags <- u2

  this <- cpLookup =<< u2
  supr <- cpLookup =<< u2

  -- update version, access, this and super
  ST.modify $ \cf ->
    cf { classVersion = version
       , classFlags = flags
       , classThis = this
       , classSuper = supr }

  ilen <- u2 -- interfaces
  when (ilen > 0) $ error "Interfaces not yet supported"

  fieldsP
  methodsP

  return ()


parseClassFile :: B.ByteString -> ClassFile
parseClassFile bs = ST.execState (runPT classFile () "" bs) emptyST
  where
    emptyST = ClassFile { classConstants  = M.empty
                        , classInterfaces = []
                        , classMethods    = []
                        , classFields     = []
                        }

test :: IO ()
test = do
  bytes <- B.readFile "aa.class"
  print $ parseClassFile bytes