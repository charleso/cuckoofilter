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
import           Data.List (elemIndex)
import           Data.List.Split
import           Data.Maybe
import qualified Data.Map as M

import           Prelude hiding (lookup)


--- Constants ---

bucketSize :: Int
bucketSize = 4

maxNumKicks :: Int
maxNumKicks = 500


--- API ---

empty :: Int -> CuckooFilter
empty n = CuckooFilter n M.empty

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
  let randomEntry = (BucketIndex (bucket i cf) 0) -- 0 to bucketSize
      -- Keep track of the previous value
      v = getFP randomEntry cf
      cf' = add f randomEntry cf
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
   in findF f (bucket i1 cf) cf <|> findF f (bucket i2 cf) cf



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
render (CuckooFilter _ m) = unlines . fmap show . chunksOf bucketSize . M.toList $ m

fromList :: (Hashable a, Show a) => Int -> [a] -> Maybe CuckooFilter
fromList n =
  foldM (flip insert) (empty n)


--- Private ---

newtype Bucket = Bucket Int deriving (Eq, Ord, Show)

type Hash = Int

data BucketIndex =
    BucketIndex Bucket Int
  deriving (Eq, Show)

newtype Fingerprint =
    Fingerprint String
  deriving (Eq, Ord, Show)

type Fingerprints = M.Map Int Fingerprint

data CuckooFilter =
    CuckooFilter Int (M.Map Bucket Fingerprints)
  deriving (Eq, Show)

hash' :: Hashable a => a -> Hash
hash' x =
  hash x

bucket :: Hash -> CuckooFilter -> Bucket
bucket x (CuckooFilter n _) =
  Bucket $ x `mod` n

findEmpty :: Bucket -> CuckooFilter -> Maybe BucketIndex
findEmpty b (CuckooFilter _ cf) =
  let b' = M.size . fromMaybe M.empty $ M.lookup b cf
  in if b' < bucketSize then Just (BucketIndex b b') else Nothing

findF :: Fingerprint -> Bucket -> CuckooFilter -> Maybe BucketIndex
findF f b (CuckooFilter _ cf) = do
  m <- M.lookup b $ cf
  fi <- elemIndex f $ M.elems m
  pure $ BucketIndex b fi

remove :: BucketIndex -> CuckooFilter -> CuckooFilter
remove (BucketIndex b fi) (CuckooFilter max' m) =
  CuckooFilter max' $ M.update (Just . M.delete fi) b m

getFP :: BucketIndex -> CuckooFilter -> Fingerprint
getFP (BucketIndex b b') (CuckooFilter _ m) =
  -- TODO Remove fromJust!
  fromJust . M.lookup b' . fromMaybe M.empty . M.lookup b $ m

add :: Fingerprint -> BucketIndex -> CuckooFilter -> CuckooFilter
add f (BucketIndex b b') (CuckooFilter max' cf) =
  CuckooFilter max' . M.insertWith M.union b (M.singleton b' f) $ cf

-- Need to make sure this isn't exaclty the same as the first hash, otherwise it's useless
fingerprint :: Show a => a -> Fingerprint
fingerprint = Fingerprint . show

instance Hashable Fingerprint where
  hash (Fingerprint f) = hash f
  hashWithSalt i (Fingerprint f) = hashWithSalt i f
