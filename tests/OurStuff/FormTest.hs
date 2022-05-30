{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}
module OurStuff.FormTest where

import Test.Hspec
import Data.Type.Equality (testEquality, (:~~:)(HRefl),type  (:~:) (Refl))
import Ourstuff.Form as F
import Type.Reflection (TypeRep, typeOf, eqTypeRep)
import GHC.IO.Handle (NewlineMode(inputNL))

newtype FormResult a = FormResult a

deriving newtype instance Show a => Show (FormResult a)
deriving newtype instance Eq a => Eq (FormResult a)

spec_Form :: Spec
spec_Form = do
    it "parses a simple text field" do
        let form = F.text @"foo"
            input = [("foo", "i guess")]
            (_, result) = F.runForm form input
        result `shouldBe` Just ("i guess" :: Text)
    it "parses a simple number field" do
        let form = F.number @"zxcv"
            input = [("zxcv", "38")]
            (view, result) = F.runForm form input
        result `shouldBe` Just 38
        F.getFromView @"zxcv" view `shouldBe` F.Parsed "38" 38
    it "can combine two fields" do
        let form = (,) F.<$> F.text @"foo" F.<*> F.text @"bar"
            input = [("foo", "jippie"), ("bar", "yeah")]
            (_, result) = F.runForm form input
        result `shouldBe` Just ("jippie", "yeah")
    it "should fail if the field isn't there" do
        let (_, result) = F.runForm (text @"foo") [("bar", "yo")]
        result `shouldBe` Nothing
    it "should be renderable" do
        let form = (,) F.<$> (const () F.<$> F.text @"foo") F.<*> F.number @"bar"
        let (view, result) = runForm form [("foo", "yea"), ("bar", "42")]
        let i = F.getFromView @"bar" view
        i `shouldBe` F.Parsed "42" 42
        result `shouldBe` Just ((), 42)
        let (emptyView, _) = runForm form []
        F.getFromView @"foo" emptyView `shouldBe` Missing
