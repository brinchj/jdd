{-# LANGUAGE FlexibleContexts
           , OverloadedStrings
  #-}

module CustomPrelude
       ( -- * Module exports
         module BasicPrelude
       , module Safe
       , Show(..)
         -- * Text-compatible functions
       , failT
       , errorT
       , showT
         -- * Ad-hoc debugging
       , trace
       , traceShow
       , traceShowId
         -- * Text/Unicode helpers
       , fromUtf8
       , toUtf8
       , fromText
         -- * Monads
       , modifyFst
       , modifySnd
         -- * FilePath
       , splitDirectories
       , pathString
       , stringPath
       , pathText
       , textPath
       )
       where


import Prelude()
import BasicPrelude hiding (show, typeOf)
import GHC.Show (Show(..))

import Safe
import Debug.Trace (trace, traceShow)

import qualified Data.Text          as T
import qualified Data.Text.Encoding as E

import qualified Filesystem.Path.CurrentOS as Path

import qualified Control.Monad.State as ST

import Filesystem.Path (splitDirectories)


-- | Ad-hoc debugging

traceShowId a = traceShow a a

-- | Text-compatible functions

failT :: Monad m => Text -> m a
failT = fail . T.unpack

errorT :: Text -> a
errorT = error . T.unpack

showT :: Show a => a -> Text
showT = T.pack . show

fromUtf8 :: ByteString -> Text
fromUtf8 = E.decodeUtf8

toUtf8 :: Text -> ByteString
toUtf8 = E.encodeUtf8

fromText :: IsString s => Text -> s
fromText = fromString . T.unpack


-- | Monads

modifyFst :: ST.MonadState (a, b) m => (a -> a) -> m ()
modifyFst f = ST.modify $ first f

modifySnd :: ST.MonadState (a, b) m => (b -> b) -> m ()
modifySnd f = ST.modify $ second f


-- | FilePath

pathString :: FilePath -> String
pathString = Path.encodeString

stringPath :: String -> FilePath
stringPath = Path.decodeString


pathText :: FilePath -> Text
pathText = either (errorT . ("Could not parse string: "++)) id . Path.toText

textPath :: Text -> FilePath
textPath = Path.fromText