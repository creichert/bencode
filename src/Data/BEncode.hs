-----------------------------------------------------------------------------
-- |
-- Module      :  BEncode
-- Copyright   :  (c) 2005 Jesper Louis Andersen <jlouis@mongers.org>
--                    2006 Lemmih <lemmih@gmail.com>
-- License     :  BSD3
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  believed to be stable
-- Portability :  portable
--
-- Provides a BEncode data type is well as functions for converting this
-- data type to and from a String.
--
-- Also supplies a number of properties which the module must satisfy.
-----------------------------------------------------------------------------
module Data.BEncode
  (
   -- * Data types
   BEncode(..),
   -- * Functions
   bRead,
   bShow,
   bPack
  )
where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (sort)
import Text.ParserCombinators.Parsec
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Char8 as BS
import Data.Binary

import Data.BEncode.Lexer ( Token (..), lexer )


type BParser a = GenParser Token () a

{- | The B-coding defines an abstract syntax tree given as a simple
     data type here
-}
data BEncode = BInt Integer
             | BString L.ByteString
             | BList [BEncode]
             | BDict (Map String BEncode)
               deriving (Eq, Ord, Show)

instance Binary BEncode where
    put e = put (BS.concat $ L.toChunks $ bPack e)
    get = do s <- get
             case bRead (L.fromChunks [s]) of
               Just e  -> return e
               Nothing -> fail "Failed to parse BEncoded data"

-- Source possition is pretty useless in BEncoded data. FIXME
updatePos :: (SourcePos -> Token -> [Token] -> SourcePos)
updatePos pos _ _ = pos

bToken :: Token -> BParser ()
bToken t = tokenPrim show updatePos fn
    where fn t' | t' == t = Just ()
          fn _ = Nothing

token' :: (Token -> Maybe a) -> BParser a
token' = tokenPrim show updatePos

tnumber :: BParser Integer
tnumber = token' fn
    where fn (TNumber i) = Just i
          fn _ = Nothing

tstring :: BParser L.ByteString
tstring = token' fn
    where fn (TString str) = Just str
          fn _ = Nothing

withToken :: Token -> BParser a -> BParser a
withToken tok
    = between (bToken tok) (bToken TEnd)

--------------------------------------------------------------
--------------------------------------------------------------

bInt :: BParser BEncode
bInt = withToken TInt $ fmap BInt tnumber

bString :: BParser BEncode
bString = fmap BString tstring

bList :: BParser BEncode
bList = withToken TList $ fmap BList (many bParse)

bDict :: BParser BEncode
bDict = withToken TDict $
    fmap (BDict . Map.fromAscList) (checkList =<< many bAssocList)
    where checkList lst = if lst /= sort lst
                            then fail "dictionary not sorted"
                            else return lst
          bAssocList
              = do str <- tstring
                   value <- bParse
                   return (L.unpack str,value)

bParse :: BParser BEncode
bParse = bDict <|> bList <|> bString <|> bInt

{- | bRead is a conversion routine. It assumes a B-coded string as input
     and attempts a parse of it into a BEncode data type
-}
bRead :: L.ByteString -> Maybe BEncode
bRead str = case parse bParse "" (lexer str) of
              Left _err -> Nothing
              Right b   -> Just b

-- | Render a BEncode structure to a B-coded string
bShow :: BEncode -> ShowS
bShow = bShow'
  where
    sc = showChar
    ss = showString
    sKV (k,v) = sString k (length k) . bShow' v
    sDict dict = foldr ((.) . sKV) id (Map.toAscList dict)
    sList = foldr ((.) . bShow') id
    sString str len = shows len . sc ':' . ss str
    bShow' b =
      case b of
        BInt i    -> sc 'i' . shows i . sc 'e'
        BString s -> sString (L.unpack s) (L.length s)
        BList bl  -> sc 'l' . sList bl . sc 'e'
        BDict bd  -> sc 'd' . sDict bd . sc 'e'

bPack :: BEncode -> L.ByteString
bPack be = L.fromChunks (bPack' be [])
    where intTag = BS.pack "i"
          colonTag = BS.pack ":"
          endTag = BS.pack "e"
          listTag = BS.pack "l"
          dictTag = BS.pack "d"
          sString s r = BS.pack (show (L.length s)) : colonTag : L.toChunks s ++ r
          bPack' (BInt i) r = intTag : BS.pack (show i) : endTag : r
          bPack' (BString s) r = sString s r
          bPack' (BList bl) r = listTag : foldr bPack' (endTag : r) bl
          bPack' (BDict bd) r = dictTag : foldr (\(k,v) -> sString (L.pack k) . bPack' v) (endTag : r) (Map.toAscList bd)

--check be = bShow be "" == L.unpack (bPack be)
