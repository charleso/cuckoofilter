{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Test.Cuckoo.Filter where
 
import           Cuckoo.Filter as CF

import           Data.Foldable as F

import           Test.QuickCheck


prop_insert :: String -> Bool
prop_insert sl =
  F.and . fmap (CF.lookup sl) $ CF.fromList 100 [sl]

prop_lookup_empty :: Int -> Bool
prop_lookup_empty sl =
  not . F.and . fmap (CF.lookup sl) $ CF.fromList 100 ([] :: [Int])

-- This will actually fail every so often - not a great property
-- We should check something about the size and expectation of failure
prop_insert_many :: Property
prop_insert_many = forAll arbitrary $ \(sl :: [Int]) ->
  fmap (\cf -> fmap (flip CF.lookup cf) sl) (CF.fromList 100 sl) === Just (fmap (const True) sl)

prop_delete :: String -> Bool
prop_delete sl =
  F.all fst . fmap (CF.delete sl) $ CF.fromList 100 [sl]

prop_delete_empty :: String -> Bool
prop_delete_empty sl =
  not . fst . CF.delete sl $ CF.empty 100


return []
tests :: IO Bool
tests = $quickCheckAll
