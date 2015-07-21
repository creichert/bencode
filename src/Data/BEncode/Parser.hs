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
    ( BParser
    , runParser
    , dict
    , list
    , optional
    , bstring
    , bbytestring
    , bint
    , (<|>)
    ) where


import           Control.Applicative        hiding (optional)
import           Control.Monad
import           Data.BEncode
import           Data.Either (rights)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

newtype BParser a = BParser {runParser :: (BEncode -> Either String a)}

instance Alternative BParser where
    empty = mzero
    (<|>) a b = a `mplus` b

instance MonadPlus BParser where
    mzero = fail "mzero"
    mplus (BParser a) (BParser b) = BParser $ \st ->
        case a st of
            Left _err -> b st
            ok         -> ok

instance Applicative BParser where
  pure = return
  (<*>) = ap

instance Monad BParser where
    BParser p >>= f = BParser $ \b -> p b >>= \res -> runParser (f res) b
    return = BParser . const . return
    fail = BParser . const . Left

instance Functor BParser where
    fmap f p = return . f =<< p


dict :: String -> BParser a -> BParser a
dict name (BParser p) = BParser $ \b -> case b of
    BDict bmap | (Just code) <- Map.lookup name bmap -> p code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b

list :: BParser a -> BParser [a]
-- note that if the inner parser fails on a member of the list
-- we still yield the members that successfully parsed
list (BParser p)
    = BParser $ \b -> case b of
        BList bs -> return . rights $ map p bs
        _ -> Left $ "Not a list: " ++ show b

optional :: BParser a -> BParser (Maybe a)
optional p = liftM Just p <|> return Nothing

bbytestring :: BParser L.ByteString
bbytestring = BParser $ \b -> case b of
    BString str -> return str
    _ -> Left $ "Expected BString, found: " ++ show b

bstring :: BParser String
bstring = fmap L.unpack bbytestring

bint :: BParser Integer
bint = BParser $ \b -> case b of
    BInt int -> return int
    _ -> Left $ "Expected BInt, found: " ++ show b
