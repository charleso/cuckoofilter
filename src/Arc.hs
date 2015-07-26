module Arc (
    Arc
  , maxSize
  , Arc.empty
  , insert
  , size
  , sizeL1
  , sizeL2
  , find
  , fromList
  , scanList
  , toList
  , keys
  , render
  ) where

import           Control.Applicative as Applicative
import           Control.Monad

import           Data.Cache.LRU (LRU)
import qualified Data.Cache.LRU as LRU
import           Data.Functor
import qualified Data.List as L (foldl', find)
import           Data.Maybe
import           Data.Monoid


-- | http://www.cs.cmu.edu/~15-440/READINGS/megiddo-computer2004.pdf
--   https://www.usenix.org/legacy/event/fast03/tech/full_papers/megiddo/megiddo.pdf
data Arc k v = Arc {
    arcB1 :: LRU k ()
  , arcT1 :: LRU k v
  , arcT2 :: LRU k v
  , arcB2 :: LRU k ()
  , maxSize :: Int
  , page :: Int
} deriving (Eq, Show)


empty :: Ord k => Int -> Arc k v
empty s' =
  Arc (LRU.newLRU Nothing) (LRU.newLRU Nothing) (LRU.newLRU Nothing) (LRU.newLRU Nothing) s' 0


insert :: Ord k => k -> v -> Arc k v -> Arc k v
insert k v a@(Arc b1 t1 t2 b2 c p) =
  fromMaybe case4 $ case1 <|> case2 <|> case3
  where
    case1 =
      let (t1', v1') = LRU.delete k t1
          (t2', v2') = LRU.delete k t2
      in (v1' <|> v2') $> Arc b1 t1' (LRU.insert k v t2) b2 c p
    case2 =
      valueOrEmpty (peek k b1) $
        let p' = min c (p + max 1 (LRU.size b2 `div` LRU.size b1))
            (Arc b1' t1' t2' b2' c' _) = replace p' a
        in Arc (fst $ LRU.delete k b1') t1' (LRU.insert k v t2') b2' c' p'
    case3 =
      valueOrEmpty (peek k b2) $
        let p' = max 0 (p + max 1 (LRU.size b1 `div` LRU.size b2))
            (Arc b1' t1' t2' b2' c' _) = replace p' a
        in Arc b1' t1' (LRU.insert k v t2') (fst $ LRU.delete k b2') c' p'
    case4 =
      let pushT1 (Arc b1' t1' t2' b2' c' p') = Arc b1' (LRU.insert k v t1') t2' b2' c' p'
      in pushT1 $
         if sizeL1 a == c
           then if LRU.size t1 < c
             then replace p $ Arc (fst $ LRU.pop b1) t1 t2 b2 c p
             else Arc b1 (fst $ LRU.pop t1) t2 b2 c p
         else if sizeL1 a < c && size a >= c
           then replace p $ if size a == 2 * c
             then Arc b1 t1 t2 (fst $ LRU.pop b2) c p
             else a
         else
           a
    replace p' a'@(Arc b1' t1' t2' b2' c' _) =
      if LRU.size t1' >= 1 && ((peek k b2' && LRU.size t1' == p') || LRU.size t1' > p')
        then let (t1'', Just (k', _)) = LRU.pop t1'
             in Arc (LRU.insert k' () b1') t1'' t2' b2' c' p'
        else let (t2'', km) = LRU.pop t2'
             in maybe a' (\(k', _) -> Arc b1' t1' t2'' (LRU.insert k' () b2') c' p') km

size :: Arc k v -> Int
size a =
  sizeL1 a + sizeL2 a

sizeL1 :: Arc k v -> Int
sizeL1 (Arc b1 t1 _ _ _ _) =
  LRU.size b1 + LRU.size t1

sizeL2 :: Arc k v -> Int
sizeL2 (Arc _ _ t2 b2 _ _) =
  LRU.size b2 + LRU.size t2

peek :: (Ord k, Eq k) => k -> LRU k v -> Bool
peek k = any ((==) k . fst) . LRU.toList

find :: Ord k => k -> Arc k v -> Maybe v
find k = fmap snd . L.find ((==) k . fst) . toList

fromList :: Ord k => Arc k v -> [(k, v)] -> Arc k v
fromList =
  L.foldl' (\b (k, v) -> insert k v b)

scanList :: Ord k => Arc k v -> [(k, v)] -> [Arc k v]
scanList =
  scanl (\b (k, v) -> insert k v b)

toList :: Ord k => Arc k v -> [(k, v)]
toList (Arc b1 l1 l2 b2 _ _) =
  LRU.toList l2 <> LRU.toList l1

keys :: Ord k => Arc k v -> [k]
keys (Arc b1 l1 l2 b2 _ _) =
  fmap fst (LRU.toList l2 <> LRU.toList l1) <> fmap fst (LRU.toList b1 <> LRU.toList b2)

render :: (Show k, Show v, Ord k) => Arc k v -> String
render (Arc b1 l1 l2 b2 _ _) =
                (show . reverse . fmap fst . LRU.toList) b1
    <> " | " <> (show . reverse . LRU.toList) l1
    <> " | " <> (show . LRU.toList) l2
    <> " | " <> (show . fmap fst . LRU.toList) b2

valueOrEmpty :: Alternative f => Bool -> a -> f a
valueOrEmpty b a = if b then pure a else Applicative.empty
