{-# LANGUAGE ScopedTypeVariables #-}
module Naqsha.Geometry.AngleSpec where

import Naqsha.Prelude
import Test.Hspec
import Test.Hspec.QuickCheck

import Naqsha.Geometry
import Naqsha.Instances ()

{-# ANN spec "Hlint: ignore Monoid law, right identity" #-}
{-# ANN spec "Hlint: ignore Monoid law, left identity" #-}

spec :: Spec
spec = describe "Group laws for Angle" $ do

  -- Commutative group under (+)
  prop "x <> mempty = x"                          $ \ (x :: Angle)
    -> (x <> mempty) `shouldBe` x
  prop "mempty <> x"                              $ \ (x :: Angle)
    -> (mempty <> x) `shouldBe` x

  prop "(<>) should be commutative"                $ \ (x :: Angle) y
    -> (x <> y) `shouldBe` (y <>  x)
  prop "(<>) should be associative"                $ \ (x :: Angle) y z
    -> (x <> (y <> z)) `shouldBe` ((x <> y) <> z)
  prop "x <> invert x = mempty"       $ \ (x :: Angle)
    -> (x <> invert x) `shouldBe` mempty


  let range = show (minBound :: Angle, maxBound :: Angle)
    in prop ("should be in range " ++ range) $ \ (x :: Angle) -> x >= minBound && x <= maxBound
