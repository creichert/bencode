-------------------------------------------------------------------------
-- |
-- Module      :  Haddock
-- Description :  BEncode Haddock Testing
-- Copyright   :  (c) 2014 Christopher Reichert
-- License     :  BSD3
-- Maintainer  :  Christopher Reichert <creichert07@gmail.com>
-- Stability   :  believed to be stable
-- Portability :  portable
--
-- Test Haddock coverage.


module Main (main) where


import           Language.Haskell.HLint (hlint)
import           System.Directory
import           System.Exit            (exitFailure, exitSuccess)


main :: IO ()
main = do
    createDirectoryIfMissing True "dist/doc/html"
    hints <- hlint arguments
    if null hints then exitSuccess else exitFailure


arguments :: [String]
arguments =
      [
        "src"
      , "tests"
      -- , "ignore=Use map"
      -- , "quiet"

        -- Leave a report in the html
      , "--report=dist/doc/html/hlint_report.html"
      ]
