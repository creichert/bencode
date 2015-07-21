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

import           Control.Monad.Trans.Reader (Reader, reader, runReader)
import           Data.Traversable           (sequenceA)
import           Control.Applicative        ((<|>))
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import           Data.BEncode

type BReader a = Reader BEncode (Either String a)

dict :: String -> BReader a -> BReader a
dict name br = reader $ \b -> case b of
    BDict bmap | (Just code) <- Map.lookup name bmap -> runReader br code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b

list :: BReader a -> BReader [a]
list br = reader $ \b -> case b of
    BList bs -> sequenceA $ map (runReader br) bs
    _ -> Left $ "Not a list: " ++ show b

optional :: BReader a -> BReader (Maybe a)
optional = fmap eitherToMaybe
    where
        eitherToMaybe (Right x) = Right $ Just x
        eitherToMaybe _ = Right Nothing

bbytestring :: BReader L.ByteString
bbytestring = reader $ \b -> case b of
    BString str -> return str
    _ -> Left $ "Expected BString, found: " ++ show b

bstring :: BReader String
bstring = (fmap . fmap) L.unpack bbytestring

bint :: BReader Integer
bint = reader $ \b -> case b of
    BInt int -> return int
    _ -> Left $ "Expected BInt, found: " ++ show b
