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
    , token
    , dict
    , list
    , optional
    , bstring
    , bbytestring
    , bint
    , setInput
    , (<|>)
    ) where


import           Control.Applicative        hiding (optional, (<|>))
import           Control.Monad
import           Data.BEncode
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

data BParser a
    = BParser (BEncode -> Reply a)

instance MonadPlus BParser where
    mzero = BParser $ \_ -> Error "mzero"
    mplus (BParser a) (BParser b) = BParser $ \st -> case a st of
                                                       Error _err -> b st
                                                       ok         -> ok


runB :: BParser a -> BEncode -> Reply a
runB (BParser b) = b

data Reply a
    = Ok a BEncode
    | Error String


instance Applicative BParser where
  pure = return
  (<*>) = ap

instance Monad BParser where
    (BParser p) >>= f = BParser $ \b -> case p b of
                                          Ok a b' -> runB (f a) b'
                                          Error str -> Error str
    return val = BParser $ Ok val
    fail str = BParser $ \_ -> Error str

instance Functor BParser where
    fmap fn p = do a <- p
                   return (fn a)

(<|>) :: BParser a -> BParser a -> BParser a
a <|> b = a `mplus` b

runParser :: BParser a -> BEncode -> Either String a
runParser parser b = case runB parser b of
                       Ok a _ -> Right a
                       Error str -> Left str

token :: BParser BEncode
token = BParser $ \b -> Ok b b

dict :: String -> BParser BEncode
dict name = BParser $ \b -> case b of
                              BDict bmap | Just code <- Map.lookup name bmap
                                   -> Ok code b
                              BDict _ -> Error $ "Name not found in dictionary: " ++ name
                              _ -> Error $ "Not a dictionary: " ++ name

list :: String -> BParser a -> BParser [a]
list name p
    = dict name >>= \lst ->
      BParser $ \b -> case lst of
                      BList bs -> foldr (cat . runB p) (Ok [] b) bs
                      _ -> Error $ "Not a list: " ++ name
    where cat (Ok v _) (Ok vs b) = Ok (v:vs) b
          cat (Ok _ _) (Error str) = Error str
          cat (Error str) _ = Error str

optional :: BParser a -> BParser (Maybe a)
optional p = liftM Just p <|> return Nothing

bstring :: BParser BEncode -> BParser String
bstring p = do b <- p
               case b of
                 BString str -> return (L.unpack str)
                 _ -> fail $ "Expected BString, found: " ++ show b

bbytestring :: BParser BEncode -> BParser L.ByteString
bbytestring p = do b <- p
                   case b of
                     BString str -> return str
                     _ -> fail $ "Expected BString, found: " ++ show b

bint :: BParser BEncode -> BParser Integer
bint p = do b <- p
            case b of
              BInt int -> return int
              _ -> fail $ "Expected BInt, found: " ++ show b

setInput :: BEncode -> BParser ()
setInput b = BParser $ \_ -> Ok () b
