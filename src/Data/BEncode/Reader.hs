{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BEncode.Reader
-- Copyright   :  (c) 2005 Lemmih <lemmih@gmail.com>
-- License     :  BSD3
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- A reader monad for BEncoded data
-----------------------------------------------------------------------------
module Data.BEncode.Reader
    ( BReader
    , runReader
    , dict
    , list
    , bstring
    , bbytestring
    , bint
    , optional
    , (<|>)
    ) where

import           Control.Applicative        (Applicative, Alternative, (<|>))
import           Control.Monad              (MonadPlus)
import           Control.Monad.Trans.Reader (Reader, reader, runReader)
import           Control.Monad.Trans.Error  (ErrorT(..), runErrorT)
import           Data.Traversable           (sequenceA)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import           Data.BEncode

newtype BReader a = BReader (ErrorT String (Reader BEncode) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

breader :: (BEncode -> (Either String a)) -> BReader a
breader = BReader . ErrorT . reader

runBReader :: BReader a -> BEncode -> Either String a
runBReader (BReader br) = runReader $ runErrorT br

dict :: String -> BReader a -> BReader a
dict name br = breader $ \b -> case b of
    BDict bmap | (Just code) <- Map.lookup name bmap -> runBReader br code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b

list :: BReader a -> BReader [a]
list br = breader $ \b -> case b of
    BList bs -> sequenceA $ map (runBReader br) bs
    _ -> Left $ "Not a list: " ++ show b

optional :: BReader a -> BReader (Maybe a)
optional br = breader $ \b -> case runBReader br b of
    Right x -> Right $ Just x
    _ -> Right Nothing

bbytestring :: BReader L.ByteString
bbytestring = breader $ \b -> case b of
    BString str -> return str
    _ -> Left $ "Expected BString, found: " ++ show b

bstring :: BReader String
bstring = fmap L.unpack bbytestring

bint :: BReader Integer
bint = breader $ \b -> case b of
    BInt int -> return int
    _ -> Left $ "Expected BInt, found: " ++ show b
