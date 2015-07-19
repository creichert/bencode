-----------------------------------------------------------------------------
-- |
-- Module      :  BParser
-- Copyright   :  (c) 2005 Lemmih <lemmih@gmail.com>
-- License     :  BSD3
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  stable
-- Portability :  portable
--
-- A parsec style parser for BEncoded data
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

newtype BParser a = BParser (BEncode -> Reply a)

instance Alternative BParser where
    empty = mzero
    (<|>) a b = a `mplus` b

instance MonadPlus BParser where
    mzero = fail "mzero"
    mplus (BParser a) (BParser b) = BParser $ \st ->
        case a st of
            Left _err -> b st
            ok         -> ok

type Reply = Either String

runParser :: BParser a -> BEncode -> Reply a
runParser (BParser b) = b

instance Applicative BParser where
  pure = return
  (<*>) = ap

instance Monad BParser where
    (BParser p) >>= f = BParser $ \b ->
        case p b of
            Right a -> runParser (f a) b
            Left str -> Left str
    return = BParser . const . return
    fail = BParser . const . fail

instance Functor BParser where
    fmap fn p = return . fn =<< p

dict :: String -> BParser a -> BParser a
dict name p = BParser $ \b -> case b of
    BDict bmap | Just code <- Map.lookup name bmap
       -> runParser p code
    BDict _ -> Left $ "Name not found in dictionary: " ++ name
    _ -> Left $ "Not a dictionary: " ++ show b

list :: BParser a -> BParser [a]
list p
    = BParser $ \b -> case b of
        -- note that if the inner parser fails on a member of the list
        -- we still yield the members that successfully parsed
        BList bs -> return . rights $ map (runParser p) bs
        _ -> Left $ "Not a list: " ++ show b

optional :: BParser a -> BParser (Maybe a)
optional p = liftM Just p <|> return Nothing

bstring :: BParser String
bstring = BParser $ \b -> case b of
    BString str -> return $ L.unpack str
    _ -> fail $ "Expected BString, found: " ++ show b

bbytestring :: BParser L.ByteString
bbytestring = BParser $ \b -> case b of
    BString str -> return str
    _ -> fail $ "Expected BString, found: " ++ show b

bint :: BParser Integer
bint = BParser $ \b -> case b of
    BInt int -> return int
    _ -> fail $ "Expected BInt, found: " ++ show b
