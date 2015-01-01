-----------------------------------------------------------------------------
-- |
-- Module      :  BEncode.Lexer
-- Copyright   :  (c) 2005 Jesper Louis Andersen <jlouis@mongers.org>,
--                         Lemmih <lemmih@gmail.com>
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


lexer :: L.ByteString -> [Token]
lexer fs | L.null fs = []
lexer fs
    = case ch of
        'd' -> TDict : lexer rest
        'l' -> TList : lexer rest
        'i' -> TInt  : lexer rest
        'e' -> TEnd  : lexer rest
        '-' -> let (digits,rest') = L.span isDigit rest
                   number = read (L.unpack digits)
               in TNumber (-number) : lexer rest'
        _ | isDigit ch
              -> let (digits,rest') = L.span isDigit fs
                     number = read (L.unpack digits)
                 in if L.null rest'
                       then [TNumber number]
                       else case L.head rest' of
                              ':' -> let (str, rest'') = L.splitAt (fromIntegral number) (L.tail rest')
                                     in TString str : lexer rest''
                              _ -> TNumber number : lexer rest'
          | otherwise -> error "Lexer error."
    where ch = L.head fs
          rest = L.tail fs
