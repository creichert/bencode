-------------------------------------------------------------------------
-- |
-- Module      :  DocTest.hs
-- Description :  BEncode Documentation Testing
-- Copyright   :  (c) 2015 Christopher Reichert
-- License     :  BSD3
-- Maintainer  :  Christopher Reichert <creichert07@gmail.com>
-- Stability   :  believed to be stable
-- Portability :  portable
--
-- DocTest coverage and verification


module Main (main) where


import           System.FilePath.Glob (glob)
import           Test.DocTest         (doctest)


main :: IO ()
main = glob "src/**/[A-Z]*.hs" >>= doctest
