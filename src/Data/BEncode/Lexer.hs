-----------------------------------------------------------------------------
-- |
-- Module      :  Data.BEncode.Lexer
-- Copyright   :  (c) 2005 Jesper Louis Andersen <jlouis@mongers.org>
--                    2006 Lemmih <lemmih@gmail.com>
-- License     :  BSD3
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  believed to be stable
-- Portability :  portable
-----------------------------------------------------------------------------
module Data.BEncode.Lexer where

import Data.Char

import qualified Data.ByteString.Lazy.Char8 as L

data Token
    = TDict
    | TList
    | TInt
    | TString L.ByteString
    | TNumber Integer
    | TEnd
      deriving (Show,Eq)


isCanonicalDigits :: L.ByteString -> Bool
isCanonicalDigits digits =
    not (L.null digits) && (digits == L.singleton '0' || L.head digits /= '0')

lexer :: L.ByteString -> [Token]
lexer fs | L.null fs = []
lexer fs
    = case ch of
        'd' -> TDict : lexer rest
        'l' -> TList : lexer rest
        'i' -> TInt  : lexer rest
        'e' -> TEnd  : lexer rest
        '-' -> let (digits,rest') = L.span isDigit rest
               in 
                  -- "-0" is not canonical bencode, so bare "0" is rejected here
                  if digits /= L.singleton '0' && isCanonicalDigits digits
                     then TNumber (negate (read (L.unpack digits))) : lexer rest'
                     else []
        _ | isDigit ch
              -> let (digits,rest') = L.span isDigit fs
                 in if isCanonicalDigits digits
                       then let number = read (L.unpack digits)
                            in if L.null rest'
                                  then [TNumber number]
                                  else case L.head rest' of
                                         ':' -> let (str, rest'') = L.splitAt (fromIntegral number) (L.tail rest')
                                                in TString str : lexer rest''
                                         _ -> TNumber number : lexer rest'
                       else []
          | otherwise -> error "Lexer error."
    where ch = L.head fs
          rest = L.tail fs
