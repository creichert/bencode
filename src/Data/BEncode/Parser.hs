-----------------------------------------------------------------------------
-- |
-- Module      :  BParser
-- Copyright   :  (c) 2005 Lemmih <lemmih@gmail.com>
-- License     :  BSD3
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- Parser combinators for BEncoded data
-----------------------------------------------------------------------------
module Data.BEncode.Parser
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


import           Control.Applicative        (optional, (<|>))
import           Control.Monad.Trans.Reader (Reader, reader, runReader)
import           Data.BEncode
import           Data.Either (rights)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

type BReader a = Reader BEncode (Either String a)

dict :: String -> BReader a -> BReader a
dict name br = reader $ \b -> case b of
    BDict bmap | (Just code) <- Map.lookup name bmap -> runReader br code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b

list :: BReader a -> BReader [a]
-- note that if the inner parser fails on a member of the list
-- we still yield the members that successfully parsed
list br = reader $ \b -> case b of
    BList bs -> return . rights $ map (runReader br) bs
    _ -> Left $ "Not a list: " ++ show b

bstring :: BReader String
bstring = reader $ \b -> case b of
    BString str -> return $ L.unpack str
    _ -> Left $ "Expected BString, found: " ++ show b

bbytestring :: BReader L.ByteString
bbytestring = reader $ \b -> case b of
    BString str -> return str
    _ -> Left $ "Expected BString, found: " ++ show b

bint :: BReader Integer
bint = reader $ \b -> case b of
    BInt int -> return int
    _ -> Left $ "Expected BInt, found: " ++ show b
