{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_HADDOCK show-extensions #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BEncode.Reader
-- Copyright   :  (c) 2015 Matthew Leon <ml@matthewleon.com>
-- License     :  BSD3
-- Maintainer  :  creichert07@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Reader monad and combinators for BEncoded data.
--
-- This is intended to replace the older "Data.BEncode.Parser" module.
--
-- Usage example:
--
-- >>> :set -XOverloadedStrings
-- >>> let bd = (BDict $ Map.fromList [("foo", BString "bar"), ("baz", BInt 1)])
-- >>> :{
-- let bReader = do
--       foo <- dict "foo" bstring
--       baz <- dict "baz" bint
--       shouldBeNothing <- optional $ dict "optionalKey" bint
--       return (foo, baz, shouldBeNothing)
-- in runBReader bReader bd
-- :}
-- Right ("bar",1,Nothing)
-----------------------------------------------------------------------------

module Data.BEncode.Reader (
    -- * Reader Monad
    BReader, runBReader,
    -- * Combinators
    bint, bbytestring, bstring, optional, list, dict
    ) where

import           Control.Applicative        (Applicative, Alternative)
import           Control.Monad              (MonadPlus)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import           Data.BEncode

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

newtype BReader a = BReader (ExceptT String (Reader BEncode) a)
    deriving (Functor, Applicative, Alternative, Monad, MonadPlus)
-- ^Reader monad for extracting data from a BEncoded structure.

breader :: (BEncode -> (Either String a)) -> BReader a
breader = BReader . ExceptT . reader
-- ^BReader constructor. Private.

runBReader :: BReader a -> BEncode -> Either String a
runBReader (BReader br) = runReader $ runExceptT br
-- ^Run a BReader. See usage examples elsewhere in this file.

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

bbytestring :: BReader L.ByteString
bbytestring = breader $ \b -> case b of
    BString str -> return str
    _ -> Left $ "Expected BString, found: " ++ show b
-- ^ Usage same as bstring, below.
-- (sadly, doctests for this cause errors on GHC 7.4)

bstring :: BReader String
bstring = fmap L.unpack bbytestring
-- ^
-- >>> runBReader bstring (BString "foo")
-- Right "foo"
--

bint :: BReader Integer
bint = breader $ \b -> case b of
    BInt int -> return int
    _ -> Left $ "Expected BInt, found: " ++ show b
-- ^
-- >>> runBReader bint (BInt 42)
-- Right 42
--

optional :: BReader a -> BReader (Maybe a)
optional br = breader $ \b -> case runBReader br b of
    Right x -> Right $ Just x
    _ -> Right Nothing
-- ^ Wrap a BReader's result in a Maybe and never fail.
--
-- >>> runBReader (optional bint) (BInt 1) 
-- Right (Just 1)
--
-- >>> runBReader (optional bint) (BString "foo")
-- Right Nothing
--
-- >>> runBReader (optional bstring) (BInt 1)
-- Right Nothing
--
-- >>> runBReader (optional bstring) (BString "foo")
-- Right (Just "foo")

list :: BReader a -> BReader [a]
list br = breader $ \b -> case b of
    BList bs -> mapM (runBReader br) bs
    _ -> Left $ "Not a list: " ++ show b
-- ^ Read a list of BEncoded data
--
-- >>> runBReader (list bint) (BList [BInt 1, BInt 2])
-- Right [1,2]
--
-- >>> runBReader (list bint) (BList [])
-- Right []
--
-- >>> let bs = (BList [BList [BString "foo", BString "bar"], BList []])
-- >>> runBReader (list $ list bstring) bs
-- Right [["foo","bar"],[]]

dict :: String -> BReader a -> BReader a
dict name br = breader $ \b -> case b of
    BDict bmap | (Just code) <- Map.lookup name bmap -> runBReader br code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b
-- ^ Read the values of a BDict corresponding to a string key
--
-- >>> let bd = (BDict $ Map.fromList [("foo", BInt 1), ("bar", BInt 2)])
-- >>> runBReader (dict "foo" bint) bd
-- Right 1
--
--
-- >>> :{
-- let bs = (BList [BDict $ Map.fromList [("foo", BString "bar"),
--                                       ("baz", BInt 2)],
--                  BDict $ Map.singleton "foo" (BString "bam")])
-- in runBReader (list $ dict "foo" bstring) bs
-- :}
-- Right ["bar","bam"]
--
-- >>> :{
-- let bd = (BDict $ Map.singleton "foo" (BList [
--             BString "foo", BString "bar"
--          ]))
-- in runBReader (dict "foo" $ list $ bstring) bd
-- :}
-- Right ["foo","bar"]
