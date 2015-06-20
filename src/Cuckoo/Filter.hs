module Cuckoo.Filter (
    empty
  , insert
  , lookup
  , delete
  , fromList
  , render
  ) where

import           Control.Applicative hiding (empty)
import           Control.Monad

import           Data.Bits
import           Data.Hashable
import           Data.Int
import           Data.List (find)
import           Data.List.Split
import           Data.Maybe
import           Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import           Prelude hiding (lookup)


type Bucket = Int
type BucketIndex = Int
type Hash = Int
type Fingerprint = String
type CuckooFilter = Vector Fingerprint

--- Constants ---

bucketSize :: Int
bucketSize = 4

missing :: Fingerprint
missing = ""

maxNumKicks :: Int
maxNumKicks = 500


--- API ---

empty :: Int -> CuckooFilter
empty n = V.replicate (n * bucketSize) missing

{-
f = fingerprint(x);
i1 = hash(x);
i2 = i1 ⊕ hash(f);
if bucket[i1] or bucket[i2] has an empty entry then
  add f to that bucket;
  return Done;
-- must relocate existing items;
i = randomly pick i1 or i2;
for n = 0; n < MaxNumKicks; n++ do
  randomly select an entry e from bucket[i];
  swap f and the fingerprint stored in entry e;
  i = i ⊕ hash(f);
  if bucket[i] has an empty entry then
    add f to bucket[i];
    return Done;
// Hashtable is considered full;
return Failure;
-}
insert :: (Hashable a, Show a) => a -> CuckooFilter -> Maybe CuckooFilter
insert x cf =
  let f = fingerprint x
      i1 = hash' x
      i2 = i1 `xor` hash' x
      b1 = bucket i1 cf
      b2 = bucket i2 cf
      b = findEmpty b1 cf <|> findEmpty b2 cf
      -- Should be random
      i = i1
   in case b of
     Just b' ->
       Just $ add f b' cf
     Nothing ->
       insertSwap maxNumKicks i f cf


insertSwap :: Int -> Hash -> Fingerprint -> CuckooFilter -> Maybe CuckooFilter
insertSwap depth i f cf =
      -- For some definition of "random"
  let randomEntry = bucket i cf
      -- Keep track of the previous value
      v = cf ! randomEntry
      cf' = cf // [(randomEntry, f)]
      i' = i `xor` hash f
      b = findEmpty (bucket i' cf) cf
   in
     if depth < 0
       then Nothing
     else
       case b of
         Just b' ->
           Just $ add v b' cf'
         Nothing ->
           insertSwap (depth - 1) i' v cf'



{-
f = fingerprint(x);
i1 = hash(x);
i2 = i1 ⊕ hash(f);
if bucket[i1] or bucket[i2] has f then
  return True;
return False;
-}
lookup :: (Hashable a, Show a) => a -> CuckooFilter -> Bool
lookup x =
  isJust . lookup' x

lookup' :: (Hashable a, Show a) => a -> CuckooFilter -> Maybe BucketIndex
lookup' x cf =
  let f = fingerprint x
      i1 = hash' x
      i2 = i1 `xor` hash f
   in hasFingerprint f (bucket i1 cf) cf <|> hasFingerprint f (bucket i2 cf) cf



{-
f = fingerprint(x);
i1 = hash(x);
i2 = i1 ⊕ hash(f);
if bucket[i1] or bucket[i2] has f then
  remove a copy of f from this bucket;
  return True;
return False;
-}
delete :: (Hashable a, Show a) => a -> CuckooFilter -> (Bool, CuckooFilter)
delete x cf =
  case lookup' x cf of
    Just b' ->
      (True, remove b' cf)
    Nothing ->
      (False, cf)


--- Utils ---

render :: CuckooFilter -> String
render = unlines . fmap show . chunksOf bucketSize . V.toList

fromList :: (Hashable a, Show a) => [a] -> Maybe CuckooFilter
fromList =
  -- TODO Pick a better size
  foldM (flip insert) (empty 100)


--- Private ---

hash' :: Hashable a => a -> Hash
hash' x =
  hash x

bucket :: Hash -> CuckooFilter -> Bucket
bucket x cf =
  x `mod` (V.length cf `div` bucketSize) * bucketSize

findEmpty :: Bucket -> CuckooFilter -> Maybe BucketIndex
findEmpty b cf =
  find (\i -> missing == cf ! i) [b..(b + bucketSize - 1)]

hasFingerprint :: Fingerprint -> Bucket -> CuckooFilter -> Maybe BucketIndex
hasFingerprint f b cf =
  find (\i -> f == cf ! i) [b..(b + bucketSize - 1)]

add :: Fingerprint -> BucketIndex -> CuckooFilter -> CuckooFilter
add f b cf =
  cf // [(b, f)]

remove :: BucketIndex -> CuckooFilter -> CuckooFilter
remove b cf =
  cf // [(b, missing)]

-- Need to make sure this isn't exaclty the same as the first hash, otherwise it's useless
fingerprint :: Show a => a -> Fingerprint
fingerprint = show
