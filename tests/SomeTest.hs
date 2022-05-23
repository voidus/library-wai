module SomeTest where

import qualified Data.Multimap as Mmap
import Ourstuff.CreateItems (focusPrefix)
import Test.Hspec
import Data.Multimap (Multimap)


spec_foo :: Spec
spec_foo =
    describe "focusPrefix" do
        it "preserves empty lists" do
            focusPrefix @[] @Int "foo" Mmap.empty `shouldBe` Mmap.empty
        it "strips prefix from items" do
            let input, expected :: Multimap [] Text Int
                input = Mmap.fromList [("yoa", 3)]
                expected = Mmap.fromList [("a", 3)]
            focusPrefix "yo" input `shouldBe` expected
        it "removes values not matching the prefix" do
            let input, expected :: Multimap [] Text Int
                input = Mmap.fromList [("x", 2)]
                expected = Mmap.empty
            focusPrefix "yo" input `shouldBe` expected

