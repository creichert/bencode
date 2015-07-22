{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- orphan Arbitrary instance is fine

import qualified Data.Map as Map

import Test.Hspec
import Test.QuickCheck

import Data.BEncode
import Data.BEncode.Reader
import Data.ByteString.Lazy (pack)

instance Arbitrary BEncode where arbitrary = sized bencode'

bencode' :: Int -> Gen BEncode
bencode' 0 = oneof [BInt `fmap` arbitrary, (BString . pack) `fmap` arbitrary]
bencode' n = oneof [
        BInt `fmap` arbitrary,
        (BString . pack) `fmap` arbitrary :: Gen BEncode,
        BList `fmap` (resize (n `div` 2) arbitrary),
        (BDict . Map.fromList) `fmap` (resize (n `div` 2) arbitrary)
    ]

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
    it "encodes dictionaries" $
        bRead "d3:baz3:moo3:foo3:bare"
            `shouldBe` Just
                (BDict (Map.fromList
                    [("baz",BString "moo"),("foo",BString "bar")]))
    it "encodes empty dictionaries" $
        bRead "de" `shouldBe` Just (BDict Map.empty)

  describe "Data.BEncode decoding" $ do
    it "is the inverse of encoding" $ property $ \bencode ->
        (bRead . bPack) bencode == Just bencode
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
        runBReader bint (BInt 42) `shouldBe` Right 42
    it "parses BStrings" $
        runBReader bstring (BString "foo") `shouldBe` Right "foo"
    it "parses BStrings with special characters in Haskell source" $
        runBReader bbytestring (BString "café") `shouldBe` Right "café"
    it "parses empty BLists" $
        runBReader (list bint) (BList []) `shouldBe` Right []
    it "parses BLists of BInts" $
        runBReader (list bint) (BList [BInt 1, BInt 2])
        `shouldBe` Right [1, 2]
    it "parses BLists of BStrings" $
        runBReader (list bstring) (BList [BString "foo", BString "bar"])
        `shouldBe` Right ["foo", "bar"]
    it "parses BLists of BStrings into ByteStrings" $
        runBReader (list bbytestring) 
                  (BList [BString "foo", BString "bar"])
        `shouldBe` Right ["foo", "bar"]
    it "parses nested BLists" $
        runBReader (list $ list bbytestring)
                  (BList [BList [BString "foo", BString "bar"], BList []])
        `shouldBe` Right [["foo", "bar"], []]
    it "parses BDicts" $
        runBReader (dict "foo" bint)
                  (BDict $ Map.fromList [("foo", BInt 1), ("bar", BInt 2)])
        `shouldBe` Right 1
    it "parses BLists of BDicts" $
        runBReader (list $ dict "foo" bstring)
                  (BList [
                    BDict $ Map.fromList [("foo", BString "bar"),
                                          ("baz", BInt 2)],
                    BDict $ Map.singleton "foo" (BString "bam")
                  ])
        `shouldBe` Right ["bar", "bam"]
    it "parses BDicts of BLists" $
        runBReader (dict "foo" $ list $ bstring)
                  (BDict $ Map.singleton "foo" (BList [
                    BString "foo", BString "bar"
                  ]))
        `shouldBe` Right ["foo", "bar"]
    it "parses optional BInts" $ do
        runBReader (optional bint) (BInt 1) 
            `shouldBe` Right (Just 1)
        runBReader (optional bint) (BString "foo")
            `shouldBe` Right Nothing
    it "parses optional BStrings" $ do
        runBReader (optional bstring) (BInt 1) `shouldBe` Right Nothing
        runBReader (optional bstring) (BString "foo")
            `shouldBe` Right (Just "foo")
    it "parses optional BDict keys" $ do
        runBReader (optional $ dict "foo" bint)
                  (BDict $ Map.fromList [("foo", BInt 1), ("bar", BInt 2)])
            `shouldBe` Right (Just 1)
        runBReader (optional $ dict "foo" bint)
                  (BDict $ Map.fromList [("bar", BInt 2)])
            `shouldBe` Right Nothing
    it "parses monadically" $ do
        parse (BDict $ Map.fromList [("foo", BString "bar"), ("baz", BInt 1)])
            `shouldBe` Right ("bar", 1)
          where 
        parse = runBReader $ do
            foo <- dict "foo" bstring
            baz <- dict "baz" bint
            return (foo, baz)
