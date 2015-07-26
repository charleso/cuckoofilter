{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Arc where

import           Arc as A

import           Data.Functor
import           Data.List (nub)

import           Test.QuickCheck


instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (Arc k v) where
  arbitrary = arbitrary >>= \(Positive c) -> fromList (empty c) <$> arbitrary


prop_constraints :: Ord k => Arc k v -> Property
prop_constraints a =
  sizeL1 a + sizeL2 a <= 2 * (A.maxSize a) .&&. sizeL1 a <= (A.maxSize a)

prop_fromList :: (Ord k, Show k, Show v, Eq k, Eq v) => Arc k v -> Property
prop_fromList a =
  toList (fromList (empty $ A.maxSize a) (reverse $ toList a)) === toList a

prop_find :: (Ord k, Eq v, Show v) => k -> v -> Arc k v -> Property
prop_find k v a =
  find k (insert k v a) === Just v

prop_unique_keys :: Ord k => Arc k v -> Property
prop_unique_keys a =
  (length . nub) (keys a) === size a


return []
tests :: IO Bool
tests = $forAllProperties $ quickCheckWithResult (stdArgs {maxSuccess = 1000})
