{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as Map

import Test.Hspec
import Data.BEncode
import Data.BEncode.Parser

main :: IO ()
main = hspec $ do

  let bll = BList [BInt (-1), BInt 0, BInt 1, BInt 2, BInt 3, BString "four"]

  describe "Data.BEncode encoding" $ do
    it "encodes integers" $
        bRead "i42e" `shouldBe` Just (BInt 42)
    it "encodes strings" $
        bRead "3:foo" `shouldBe` Just (BString "foo")
    it "encodes strings with special characters in Haskell source" $
        bRead "5:café" `shouldBe` Just (BString "café")
    it "encodes lists" $
        bRead "l5:helloi42eli-1ei0ei1ei2ei3e4:fouree"
          `shouldBe` Just (BList [ BString "hello", BInt 42, bll ])
    it "encodes nested lists" $
        bRead "ll5:helloi62eel3:fooee"
          `shouldBe` Just (BList
            [ BList [ BString "hello", BInt 62 ],
              BList [ BString "foo" ] ])

  describe "Data.BEncode decoding" $ do
    it "decodes int" $
        bPack (BInt 42) `shouldBe` "i42e"
    it "decodes null int" $
        bPack (BInt 0) `shouldBe` "i0e"
    it "decodes negative int" $
        bPack (BInt (-42)) `shouldBe` "i-42e"
    it "decodes string" $ do
        bPack (BString "foo") `shouldBe` "3:foo"
        bPack (BString "") `shouldBe` "0:"
    it "decodes lists" $
        bPack (BList [BInt 1, BInt 2, BInt 3]) `shouldBe` "li1ei2ei3ee"
    it "decodes lists of lists" $
        bPack (BList [ BList [BInt 1], BInt 1, BInt 2, BInt 3])
          `shouldBe` "lli1eei1ei2ei3ee"
    it "decodes hash" $ do
        let d = Map.fromList [("foo", BString "bar"), ("baz",BString "qux")]
        bPack (BDict d) `shouldBe` "d3:baz3:qux3:foo3:bare"
    -- FIX
    -- it "decodes unicode" $ do
    --   bPack (BString "café") `shouldBe` "5:café"
    --   bPack (BList [BString "你好", BString "中文"]) `shouldBe` "l6:你好6:中文e"
    it "decodes lists of lists" $
        bRead "l5:helloi42eli-1ei0ei1ei2ei3e4:fouree"
            `shouldBe` Just (BList [ BString "hello", BInt 42, bll])

  describe "Data.BEncode.Parser" $ do
    it "parses BInts" $
        runParser (bint token) (BInt 42) `shouldBe` Right 42
    it "parses BStrings" $
        runParser (bstring token) (BString "foo") `shouldBe` Right "foo"
    it "parses BStrings with special characters in Haskell source" $
        runParser (bbytestring token) (BString "café") `shouldBe` Right "café"
    -- it "encodes lists" $ undefined
