{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Foldable (
    Foldable(..),
    foldrM,
    foldlM,
    traverse_,
    for_,
    sequenceA_,
    asum,
    mapM_,
    forM_,
    sequence_,
    msum,
    concat,
    concatMap,
    and,
    or,
    any,
    all,
    maximumBy,
    minimumBy,
    notElem,
    find
    ) where
import Data.Bool
import Data.Either
import Data.Eq
import qualified GHC.List as List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Proxy
import GHC.Arr  ( Array(..), elems, numElements,
                  foldlElems, foldrElems,
                  foldlElems', foldrElems',
                  foldl1Elems, foldr1Elems)
import GHC.Base hiding ( foldr )
import GHC.Num  ( Num(..) )
infix  4 `elem`, `notElem`
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}
    fold :: Monoid m => t m -> m
    fold = foldMap id
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty
    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z
    foldr' :: (a -> b -> b) -> b -> t a -> b
    foldr' f z0 xs = foldl f' id xs z0
      where f' k x z = k $! f x z
    foldl :: (b -> a -> b) -> b -> t a -> b
    foldl f z t = appEndo (getDual (foldMap (Dual . Endo . flip f) t)) z
    foldl' :: (b -> a -> b) -> b -> t a -> b
    foldl' f z0 xs = foldr f' id xs z0
      where f' x k z = k $! f z x
    foldr1 :: (a -> a -> a) -> t a -> a
    foldr1 f xs = fromMaybe (error "foldr1: empty structure")
                    (foldr mf Nothing xs)
      where
        mf x m = Just (case m of
                         Nothing -> x
                         Just y  -> f x y)
    foldl1 :: (a -> a -> a) -> t a -> a
    foldl1 f xs = fromMaybe (error "foldl1: empty structure")
                    (foldl mf Nothing xs)
      where
        mf m y = Just (case m of
                         Nothing -> y
                         Just x  -> f x y)
    toList :: t a -> [a]
    {-# INLINE toList #-}
    toList t = build (\ c n -> foldr c n t)
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True
    length :: t a -> Int
    length = foldl' (\c _ -> c+1) 0
    elem :: Eq a => a -> t a -> Bool
    elem = any . (==)
    maximum :: forall a . Ord a => t a -> a
    maximum = fromMaybe (error "maximum: empty structure") .
       getMax . foldMap (Max #. (Just :: a -> Maybe a))
    minimum :: forall a . Ord a => t a -> a
    minimum = fromMaybe (error "minimum: empty structure") .
       getMin . foldMap (Min #. (Just :: a -> Maybe a))
    sum :: Num a => t a -> a
    sum = getSum #. foldMap Sum
    product :: Num a => t a -> a
    product = getProduct #. foldMap Product
instance Foldable Maybe where
    foldr _ z Nothing = z
    foldr f z (Just x) = f x z
    foldl _ z Nothing = z
    foldl f z (Just x) = f z x
instance Foldable [] where
    elem    = List.elem
    foldl   = List.foldl
    foldl'  = List.foldl'
    foldl1  = List.foldl1
    foldr   = List.foldr
    foldr1  = List.foldr1
    length  = List.length
    maximum = List.maximum
    minimum = List.minimum
    null    = List.null
    product = List.product
    sum     = List.sum
    toList  = id
instance Foldable (Either a) where
    foldMap _ (Left _) = mempty
    foldMap f (Right y) = f y
    foldr _ z (Left _) = z
    foldr f z (Right y) = f y z
    length (Left _)  = 0
    length (Right _) = 1
    null             = isLeft
instance Foldable ((,) a) where
    foldMap f (_, y) = f y
    foldr f z (_, y) = f y z
instance Foldable (Array i) where
    foldr = foldrElems
    foldl = foldlElems
    foldl' = foldlElems'
    foldr' = foldrElems'
    foldl1 = foldl1Elems
    foldr1 = foldr1Elems
    toList = elems
    length = numElements
    null a = numElements a == 0
instance Foldable Proxy where
    foldMap _ _ = mempty
    {-# INLINE foldMap #-}
    fold _ = mempty
    {-# INLINE fold #-}
    foldr _ z _ = z
    {-# INLINE foldr #-}
    foldl _ z _ = z
    {-# INLINE foldl #-}
    foldl1 _ _ = error "foldl1: Proxy"
    foldr1 _ _ = error "foldr1: Proxy"
    length _   = 0
    null _     = True
    elem _ _   = False
    sum _      = 0
    product _  = 1
instance Foldable Dual where
    foldMap            = coerce
    elem               = (. getDual) #. (==)
    foldl              = coerce
    foldl'             = coerce
    foldl1 _           = getDual
    foldr f z (Dual x) = f x z
    foldr'             = foldr
    foldr1 _           = getDual
    length _           = 1
    maximum            = getDual
    minimum            = getDual
    null _             = False
    product            = getDual
    sum                = getDual
    toList (Dual x)    = [x]
instance Foldable Sum where
    foldMap            = coerce
    elem               = (. getSum) #. (==)
    foldl              = coerce
    foldl'             = coerce
    foldl1 _           = getSum
    foldr f z (Sum x)  = f x z
    foldr'             = foldr
    foldr1 _           = getSum
    length _           = 1
    maximum            = getSum
    minimum            = getSum
    null _             = False
    product            = getSum
    sum                = getSum
    toList (Sum x)     = [x]
instance Foldable Product where
    foldMap               = coerce
    elem                  = (. getProduct) #. (==)
    foldl                 = coerce
    foldl'                = coerce
    foldl1 _              = getProduct
    foldr f z (Product x) = f x z
    foldr'                = foldr
    foldr1 _              = getProduct
    length _              = 1
    maximum               = getProduct
    minimum               = getProduct
    null _                = False
    product               = getProduct
    sum                   = getProduct
    toList (Product x)    = [x]
instance Foldable First where
    foldMap f = foldMap f . getFirst
instance Foldable Last where
    foldMap f = foldMap f . getLast
newtype Max a = Max {getMax :: Maybe a}
newtype Min a = Min {getMin :: Maybe a}
instance Ord a => Monoid (Max a) where
  mempty = Max Nothing
  {-# INLINE mappend #-}
  m `mappend` Max Nothing = m
  Max Nothing `mappend` n = n
  (Max m@(Just x)) `mappend` (Max n@(Just y))
    | x >= y    = Max m
    | otherwise = Max n
instance Ord a => Monoid (Min a) where
  mempty = Min Nothing
  {-# INLINE mappend #-}
  m `mappend` Min Nothing = m
  Min Nothing `mappend` n = n
  (Min m@(Just x)) `mappend` (Min n@(Just y))
    | x <= y    = Min m
    | otherwise = Min n
foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b
foldrM f z0 xs = foldl f' return xs z0
  where f' k x z = f x z >>= k
foldlM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
foldlM f z0 xs = foldr f' return xs z0
  where f' x k z = f z x >>= k
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr ((*>) . f) (pure ())
for_ :: (Foldable t, Applicative f) => t a -> (a -> f b) -> f ()
{-# INLINE for_ #-}
for_ = flip traverse_
mapM_ :: (Foldable t, Monad m) => (a -> m b) -> t a -> m ()
mapM_ f= foldr ((>>) . f) (return ())
forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
{-# INLINE forM_ #-}
forM_ = flip mapM_
sequenceA_ :: (Foldable t, Applicative f) => t (f a) -> f ()
sequenceA_ = foldr (*>) (pure ())
sequence_ :: (Foldable t, Monad m) => t (m a) -> m ()
sequence_ = foldr (>>) (return ())
asum :: (Foldable t, Alternative f) => t (f a) -> f a
{-# INLINE asum #-}
asum = foldr (<|>) empty
msum :: (Foldable t, MonadPlus m) => t (m a) -> m a
{-# INLINE msum #-}
msum = asum
concat :: Foldable t => t [a] -> [a]
concat xs = build (\c n -> foldr (\x y -> foldr c y x) n xs)
{-# INLINE concat #-}
concatMap :: Foldable t => (a -> [b]) -> t a -> [b]
concatMap f xs = build (\c n -> foldr (\x b -> foldr c b (f x)) n xs)
{-# INLINE concatMap #-}
and :: Foldable t => t Bool -> Bool
and = getAll #. foldMap All
or :: Foldable t => t Bool -> Bool
or = getAny #. foldMap Any
any :: Foldable t => (a -> Bool) -> t a -> Bool
any p = getAny #. foldMap (Any #. p)
all :: Foldable t => (a -> Bool) -> t a -> Bool
all p = getAll #. foldMap (All #. p)
maximumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
maximumBy cmp = foldr1 max'
  where max' x y = case cmp x y of
                        GT -> x
                        _  -> y
minimumBy :: Foldable t => (a -> a -> Ordering) -> t a -> a
minimumBy cmp = foldr1 min'
  where min' x y = case cmp x y of
                        GT -> y
                        _  -> x
notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = not . elem x
find :: Foldable t => (a -> Bool) -> t a -> Maybe a
find p = getFirst . foldMap (\ x -> First (if p x then Just x else Nothing))
(#.) :: Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _f = coerce
{-# INLINE (#.) #-}
