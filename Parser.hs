{-# LANGUAGE OverloadedStrings, GeneralizedNewtypeDeriving #-}

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

-- Parse 1-byte, 2-byte, 4-byte and 8-byte unsigned int (big-endian)
u1, u2, u4, u8 :: Parser Integer
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

s1, s2, s4, s8 :: Parser Integer
s1 = makeSigned 8  <$> u1
s2 = makeSigned 16 <$> u2
s4 = makeSigned 32 <$> u4
s8 = makeSigned 64 <$> u8


-- Parse magic number
magicNumberP :: Parser ()
magicNumberP = void $ string "\xCA\xFE\xBA\xBE"

-- Parse version
versionP :: Parser Version
versionP = Version <$> u2 <*> u2


-- Parse a string with 2-byte length prefix
string2P :: Parser B.ByteString
string2P = B.pack <$> (flip replicateM anyChar =<< fromIntegral <$> u2)

-- Parse constant pool
type ConstantPool = M.Map Integer Constant
constantPoolP :: Parser ConstantPool
constantPoolP = do
  maxID <- fromIntegral <$> u2
  ST.execStateT (E.runErrorT $ entriesP $ maxID - 1) M.empty
  where
    entriesP :: Int -> E.ErrorT String (ST.StateT ConstantPool Parser) ()
    entriesP = flip replicateM_ entryP

    entryP = do
      constant <- convertTag =<< lift (lift u1)
      ST.modify $ \m -> M.insert (fromIntegral $ M.size m + 1) constant m

    convertTag tag =
      case tag of
        0  -> fail "EOF"
        1  -> Str              <$^> string2P
        3  -> SignedInt        <$^> s4
        5  -> Long             <$^> s8
        7  -> ClassRef . Class . unStr <$> get1

        10 -> Method  `get2` unClassRef $ unDescRef
        12 -> DescRef <$> (Desc `get2` unStr  $ unStr)

        _  -> error $ "Unknown constant pool entry-tag: " ++ show tag

      where
        f <$^> v = f <$> lift (lift v)

        get1 = do
          idx <- lift $ lift u2
          ST.gets $ fromMaybe (error "invalid id") . M.lookup idx

        get2 f ma mb = do
          a <- ma <$> get1
          b <- mb <$> get1
          return $ f a b


cpLookup cp i = fromMaybe (error $ "invalid id: " ++ show i) $ M.lookup i cp


many2 :: Parser a -> Parser [a]
many2 a = do
  n <- fromIntegral <$> u2
  replicateM n a


interfacesP = undefined

blockP cp = do
  flags <- u2
  Str name <- cpLookup cp <$> u2
  Str desc <- cpLookup cp <$> u2
  as <- many2 $ attributeP cp
  return $ AttrBlock flags name desc as

attributeP cp = do
  Str name <- cpLookup cp <$> u2
  body <- B.pack <$> (flip replicateM anyChar =<< fromIntegral <$> u4)
  return $ Attr name body


--
-- TODO: Implement a Class datatype and use a StateT to update it instead of
--       parsing parameters around (see constantPoolP)
--
classFile = do
  _ <- magicNumberP
  version <- versionP
  cp <- constantPoolP
  access <- u2

  this <- cpLookup cp <$> u2
  supr <- cpLookup cp <$> u2

  ilen <- u2 -- interfaces
  when (ilen > 0) $ error "Interfaces not yet supported"

  fields  <- many2 $ blockP cp
  methods <- many2 $ blockP cp

  return (version, access, cp, this, supr, ilen, fields, methods)


parseClassFile :: B.ByteString -> IO ()
parseClassFile = parseTest classFile

test :: IO ()
test = B.readFile "aa.class" >>= parseClassFile