{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Prelude hiding (take)


import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.State.Strict as ST

import Control.Applicative ((<$>), (<*>))


import qualified Data.ByteString as B
import qualified Data.Map as M

import Data.Maybe


import Data.Attoparsec.ByteString

data Version = Version { getMajor :: Integer, getMinor :: Integer}
             deriving Show

data Constant = Str
                { unStr :: B.ByteString }
              | SignedInt Integer
              | Long Integer
              | ClassRef
                { classPath :: B.ByteString }
              | Desc
                { descName :: B.ByteString
                , descType :: B.ByteString }
              | Method
                { methodClass :: Constant
                , methodDesc  :: Constant }
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
u1, u2, u4 :: Parser Integer
u1 = fromIntegral <$> anyWord8

u2 = do
  a <- u1
  b <- u1
  return $ (a * 2^8) + b

u4 = do
  a <- u2
  b <- u2
  return $ (a * 2^16) + b

u8 :: Parser Integer
u8 = do
  a <- fromIntegral <$> u4
  b <- fromIntegral <$> u4
  return $ (a * 2^32) + b


-- Parse 1-byte, 2-byte, 4-byte, 8-byte signed int (two complement, big-endian)
makeSigned bits n | n > 2 ^ (bits - 1) = n + (2 ^ bits)
                  | otherwise          = n

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
string2P = take =<< fromIntegral <$> u2

-- Parse constant pool
constantPoolP :: Parser (M.Map Integer Constant)
constantPoolP = do
  maxID <- u2
  ST.execStateT (entriesP $ maxID - 1) M.empty
  where
    entriesP :: Integer -> ST.StateT (M.Map Integer Constant) Parser ()
    entriesP 0 = return ()
    entriesP n = do
      tag <- lift u1
      case tag of
        0 -> return ()
        _ -> do const <- convertTag tag
                ST.modify $ \m -> M.insert (fromIntegral $ M.size m + 1) const m
                entriesP $ n - 1

    convertTag :: Integer -> ST.StateT (M.Map Integer Constant) Parser Constant
    convertTag tag =
      case tag of
        1  -> Str              <$^> string2P
        3  -> SignedInt        <$^> s4
        5  -> Long             <$^> s8
        7  -> ClassRef . unStr  <$> get1

        10 -> Method `get2` id    $ id
        12 -> Desc   `get2` unStr $ unStr

        _  -> error $ "Unknown constant pool entry-tag: " ++ show tag

      where
        f <$^> v = f <$> lift v

        get1 :: ST.StateT (M.Map Integer Constant) Parser Constant
        get1 = do
          idx <- lift u2
          ST.gets $ fromMaybe (error "invalid id") . M.lookup idx

        get2 f ma mb = do
          a <- ma <$> get1
          b <- mb <$> get1
          return $ f a b


cpLookup cp i = fromMaybe (error $ "invalid id: " ++ show i) $ M.lookup i cp


many2 f = do
  count <- fromIntegral <$> u2
  replicateM count f


interfacesP = undefined

blockP cp = do
  flags <- u2
  Str name <- cpLookup cp <$> u2
  Str desc <- cpLookup cp <$> u2
  as <- many2 $ attributeP cp
  return $ AttrBlock flags name desc as

attributeP cp = do
  Str name <- cpLookup cp <$> u2
  body <- (take . fromIntegral) =<< u4
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

  return (cp, this, supr, ilen, fields, methods)


parseClassFile = parseTest classFile

test = B.readFile "aa.class" >>= parseClassFile