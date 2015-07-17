{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP, NoImplicitPrelude, ScopedTypeVariables, MagicHash #-}
module Data.OldList
   (
     (++)
   , head
   , last
   , tail
   , init
   , uncons
   , null
   , length
   , map
   , reverse
   , intersperse
   , intercalate
   , transpose
   , subsequences
   , permutations
   , foldl
   , foldl'
   , foldl1
   , foldl1'
   , foldr
   , foldr1
   , concat
   , concatMap
   , and
   , or
   , any
   , all
   , sum
   , product
   , maximum
   , minimum
   , scanl
   , scanl'
   , scanl1
   , scanr
   , scanr1
   , mapAccumL
   , mapAccumR
   , iterate
   , repeat
   , replicate
   , cycle
   , unfoldr
   , take
   , drop
   , splitAt
   , takeWhile
   , dropWhile
   , dropWhileEnd
   , span
   , break
   , stripPrefix
   , group
   , inits
   , tails
   , isPrefixOf
   , isSuffixOf
   , isInfixOf
   , elem
   , notElem
   , lookup
   , find
   , filter
   , partition
   , (!!)
   , elemIndex
   , elemIndices
   , findIndex
   , findIndices
   , zip
   , zip3
   , zip4, zip5, zip6, zip7
   , zipWith
   , zipWith3
   , zipWith4, zipWith5, zipWith6, zipWith7
   , unzip
   , unzip3
   , unzip4, unzip5, unzip6, unzip7
   , lines
   , words
   , unlines
   , unwords
   , nub
   , delete
   , (\\)
   , union
   , intersect
   , sort
   , sortOn
   , insert
   , nubBy
   , deleteBy
   , deleteFirstsBy
   , unionBy
   , intersectBy
   , groupBy
   , sortBy
   , insertBy
   , maximumBy
   , minimumBy
   , genericLength
   , genericTake
   , genericDrop
   , genericSplitAt
   , genericIndex
   , genericReplicate
   ) where
import Data.Maybe
import Data.Bits        ( (.&.) )
import Data.Char        ( isSpace )
import Data.Ord         ( comparing )
import Data.Tuple       ( fst, snd )
import GHC.Num
import GHC.Real
import GHC.List
import GHC.Base
infix 5 \\ 
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []
stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] ys = Just ys
stripPrefix (x:xs) (y:ys)
 | x == y = stripPrefix xs ys
stripPrefix _ _ = Nothing
elemIndex       :: Eq a => a -> [a] -> Maybe Int
elemIndex x     = findIndex (x==)
elemIndices     :: Eq a => a -> [a] -> [Int]
elemIndices x   = findIndices (x==)
find            :: (a -> Bool) -> [a] -> Maybe a
find p          = listToMaybe . filter p
findIndex       :: (a -> Bool) -> [a] -> Maybe Int
findIndex p     = listToMaybe . findIndices p
findIndices      :: (a -> Bool) -> [a] -> [Int]
#ifdef USE_REPORT_PRELUDE
findIndices p xs = [ i | (x,i) <- zip xs [0..], p x]
#else
{-# INLINE findIndices #-}
findIndices p ls = build $ \c n ->
  let go x r k | p x       = I# k `c` r (k +# 1#)
               | otherwise = r (k +# 1#)
  in foldr go (\_ -> n) ls 0#
#endif  /* USE_REPORT_PRELUDE */
isPrefixOf              :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _         =  True
isPrefixOf _  []        =  False
isPrefixOf (x:xs) (y:ys)=  x == y && isPrefixOf xs ys
isSuffixOf              :: (Eq a) => [a] -> [a] -> Bool
ns `isSuffixOf` hs      = maybe False id $ do
      delta <- dropLengthMaybe ns hs
      return $ ns == dropLength delta hs
dropLength :: [a] -> [b] -> [b]
dropLength [] y = y
dropLength _ [] = []
dropLength (_:x') (_:y') = dropLength x' y'
dropLengthMaybe :: [a] -> [b] -> Maybe [b]
dropLengthMaybe [] y = Just y
dropLengthMaybe _ [] = Nothing
dropLengthMaybe (_:x') (_:y') = dropLengthMaybe x' y'
isInfixOf               :: (Eq a) => [a] -> [a] -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
nub                     :: (Eq a) => [a] -> [a]
nub                     =  nubBy (==)
nubBy                   :: (a -> a -> Bool) -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
nubBy eq []             =  []
nubBy eq (x:xs)         =  x : nubBy eq (filter (\ y -> not (eq x y)) xs)
#else
nubBy eq l              = nubBy' l []
  where
    nubBy' [] _         = []
    nubBy' (y:ys) xs
       | elem_by eq y xs = nubBy' ys xs
       | otherwise       = y : nubBy' ys (y:xs)
elem_by :: (a -> a -> Bool) -> a -> [a] -> Bool
elem_by _  _ []         =  False
elem_by eq y (x:xs)     =  x `eq` y || elem_by eq y xs
#endif
delete                  :: (Eq a) => a -> [a] -> [a]
delete                  =  deleteBy (==)
deleteBy                :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _  _ []        = []
deleteBy eq x (y:ys)    = if x `eq` y then ys else y : deleteBy eq x ys
(\\)                    :: (Eq a) => [a] -> [a] -> [a]
(\\)                    =  foldl (flip delete)
union                   :: (Eq a) => [a] -> [a] -> [a]
union                   = unionBy (==)
unionBy                 :: (a -> a -> Bool) -> [a] -> [a] -> [a]
unionBy eq xs ys        =  xs ++ foldl (flip (deleteBy eq)) (nubBy eq ys) xs
intersect               :: (Eq a) => [a] -> [a] -> [a]
intersect               =  intersectBy (==)
intersectBy             :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy _  [] _     =  []
intersectBy _  _  []    =  []
intersectBy eq xs ys    =  [x | x <- xs, any (eq x) ys]
intersperse             :: a -> [a] -> [a]
intersperse _   []      = []
intersperse sep (x:xs)  = x : prependToAll sep xs
prependToAll            :: a -> [a] -> [a]
prependToAll _   []     = []
prependToAll sep (x:xs) = sep : x : prependToAll sep xs
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
transpose               :: [[a]] -> [[a]]
transpose []             = []
transpose ([]   : xss)   = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) : transpose (xs : [ t | (_:t) <- xss])
partition               :: (a -> Bool) -> [a] -> ([a],[a])
{-# INLINE partition #-}
partition p xs = foldr (select p) ([],[]) xs
select :: (a -> Bool) -> a -> ([a], [a]) -> ([a], [a])
select p x ~(ts,fs) | p x       = (x:ts,fs)
                    | otherwise = (ts, x:fs)
mapAccumL :: (acc -> x -> (acc, y)) 
          -> acc            
          -> [x]            
          -> (acc, [y])     
{-# NOINLINE [1] mapAccumL #-}
mapAccumL _ s []        =  (s, [])
mapAccumL f s (x:xs)    =  (s'',y:ys)
                           where (s', y ) = f s x
                                 (s'',ys) = mapAccumL f s' xs
{-# RULES
"mapAccumL" [~1] forall f s xs . mapAccumL f s xs = foldr (mapAccumLF f) pairWithNil xs s
"mapAccumLList" [1] forall f s xs . foldr (mapAccumLF f) pairWithNil xs s = mapAccumL f s xs
 #-}
pairWithNil :: acc -> (acc, [y])
{-# INLINE [0] pairWithNil #-}
pairWithNil x = (x, [])
mapAccumLF :: (acc -> x -> (acc, y)) -> x -> (acc -> (acc, [y])) -> acc -> (acc, [y])
{-# INLINE [0] mapAccumLF #-}
mapAccumLF f = \x r -> oneShot (\s ->
                         let (s', y)   = f s x
                             (s'', ys) = r s'
                         in (s'', y:ys))
mapAccumR :: (acc -> x -> (acc, y))     
            -> acc              
            -> [x]              
            -> (acc, [y])               
mapAccumR _ s []        =  (s, [])
mapAccumR f s (x:xs)    =  (s'', y:ys)
                           where (s'',y ) = f s' x
                                 (s', ys) = mapAccumR f s xs
insert :: Ord a => a -> [a] -> [a]
insert e ls = insertBy (compare) e ls
insertBy :: (a -> a -> Ordering) -> a -> [a] -> [a]
insertBy _   x [] = [x]
insertBy cmp x ys@(y:ys')
 = case cmp x y of
     GT -> y : insertBy cmp x ys'
     _  -> x : ys
maximumBy               :: (a -> a -> Ordering) -> [a] -> a
maximumBy _ []          =  error "List.maximumBy: empty list"
maximumBy cmp xs        =  foldl1 maxBy xs
                        where
                           maxBy x y = case cmp x y of
                                       GT -> x
                                       _  -> y
minimumBy               :: (a -> a -> Ordering) -> [a] -> a
minimumBy _ []          =  error "List.minimumBy: empty list"
minimumBy cmp xs        =  foldl1 minBy xs
                        where
                           minBy x y = case cmp x y of
                                       GT -> y
                                       _  -> x
genericLength           :: (Num i) => [a] -> i
{-# NOINLINE [1] genericLength #-}
genericLength []        =  0
genericLength (_:l)     =  1 + genericLength l
{-# RULES
  "genericLengthInt"     genericLength = (strictGenericLength :: [a] -> Int);
  "genericLengthInteger" genericLength = (strictGenericLength :: [a] -> Integer);
 #-}
strictGenericLength     :: (Num i) => [b] -> i
strictGenericLength l   =  gl l 0
                        where
                           gl [] a     = a
                           gl (_:xs) a = let a' = a + 1 in a' `seq` gl xs a'
genericTake             :: (Integral i) => i -> [a] -> [a]
genericTake n _ | n <= 0 = []
genericTake _ []        =  []
genericTake n (x:xs)    =  x : genericTake (n-1) xs
genericDrop             :: (Integral i) => i -> [a] -> [a]
genericDrop n xs | n <= 0 = xs
genericDrop _ []        =  []
genericDrop n (_:xs)    =  genericDrop (n-1) xs
genericSplitAt          :: (Integral i) => i -> [a] -> ([a], [a])
genericSplitAt n xs | n <= 0 =  ([],xs)
genericSplitAt _ []     =  ([],[])
genericSplitAt n (x:xs) =  (x:xs',xs'') where
    (xs',xs'') = genericSplitAt (n-1) xs
genericIndex :: (Integral i) => [a] -> i -> a
genericIndex (x:_)  0 = x
genericIndex (_:xs) n
 | n > 0     = genericIndex xs (n-1)
 | otherwise = error "List.genericIndex: negative argument."
genericIndex _ _      = error "List.genericIndex: index too large."
genericReplicate        :: (Integral i) => i -> a -> [a]
genericReplicate n x    =  genericTake n (repeat x)
zip4                    :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
zip4                    =  zipWith4 (,,,)
zip5                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
zip5                    =  zipWith5 (,,,,)
zip6                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [(a,b,c,d,e,f)]
zip6                    =  zipWith6 (,,,,,)
zip7                    :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] ->
                              [g] -> [(a,b,c,d,e,f,g)]
zip7                    =  zipWith7 (,,,,,,)
zipWith4                :: (a->b->c->d->e) -> [a]->[b]->[c]->[d]->[e]
zipWith4 z (a:as) (b:bs) (c:cs) (d:ds)
                        =  z a b c d : zipWith4 z as bs cs ds
zipWith4 _ _ _ _ _      =  []
zipWith5                :: (a->b->c->d->e->f) ->
                           [a]->[b]->[c]->[d]->[e]->[f]
zipWith5 z (a:as) (b:bs) (c:cs) (d:ds) (e:es)
                        =  z a b c d e : zipWith5 z as bs cs ds es
zipWith5 _ _ _ _ _ _    = []
zipWith6                :: (a->b->c->d->e->f->g) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]
zipWith6 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs)
                        =  z a b c d e f : zipWith6 z as bs cs ds es fs
zipWith6 _ _ _ _ _ _ _  = []
zipWith7                :: (a->b->c->d->e->f->g->h) ->
                           [a]->[b]->[c]->[d]->[e]->[f]->[g]->[h]
zipWith7 z (a:as) (b:bs) (c:cs) (d:ds) (e:es) (f:fs) (g:gs)
                   =  z a b c d e f g : zipWith7 z as bs cs ds es fs gs
zipWith7 _ _ _ _ _ _ _ _ = []
unzip4                  :: [(a,b,c,d)] -> ([a],[b],[c],[d])
unzip4                  =  foldr (\(a,b,c,d) ~(as,bs,cs,ds) ->
                                        (a:as,b:bs,c:cs,d:ds))
                                 ([],[],[],[])
unzip5                  :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
unzip5                  =  foldr (\(a,b,c,d,e) ~(as,bs,cs,ds,es) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es))
                                 ([],[],[],[],[])
unzip6                  :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
unzip6                  =  foldr (\(a,b,c,d,e,f) ~(as,bs,cs,ds,es,fs) ->
                                        (a:as,b:bs,c:cs,d:ds,e:es,f:fs))
                                 ([],[],[],[],[],[])
unzip7          :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])
unzip7          =  foldr (\(a,b,c,d,e,f,g) ~(as,bs,cs,ds,es,fs,gs) ->
                                (a:as,b:bs,c:cs,d:ds,e:es,f:fs,g:gs))
                         ([],[],[],[],[],[],[])
deleteFirstsBy          :: (a -> a -> Bool) -> [a] -> [a] -> [a]
deleteFirstsBy eq       =  foldl (flip (deleteBy eq))
group                   :: Eq a => [a] -> [[a]]
group                   =  groupBy (==)
groupBy                 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy _  []           =  []
groupBy eq (x:xs)       =  (x:ys) : groupBy eq zs
                           where (ys,zs) = span (eq x) xs
inits                   :: [a] -> [[a]]
inits                   = map toListSB . scanl' snocSB emptySB
{-# NOINLINE inits #-}
tails                   :: [a] -> [[a]]
{-# INLINABLE tails #-}
tails lst               =  build (\c n ->
  let tailsGo xs = xs `c` case xs of
                             []      -> n
                             _ : xs' -> tailsGo xs'
  in tailsGo lst)
subsequences            :: [a] -> [[a]]
subsequences xs         =  [] : nonEmptySubsequences xs
nonEmptySubsequences         :: [a] -> [[a]]
nonEmptySubsequences []      =  []
nonEmptySubsequences (x:xs)  =  [x] : foldr f [] (nonEmptySubsequences xs)
  where f ys r = ys : (x : ys) : r
permutations            :: [a] -> [[a]]
permutations xs0        =  xs0 : perms xs0 []
  where
    perms []     _  = []
    perms (t:ts) is = foldr interleave (perms ts (t:is)) (permutations is)
      where interleave    xs     r = let (_,zs) = interleave' id xs r in zs
            interleave' _ []     r = (ts, r)
            interleave' f (y:ys) r = let (us,zs) = interleave' (f . (y:)) ys r
                                     in  (y:us, f (t:y:us) : zs)
sort :: (Ord a) => [a] -> [a]
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
#ifdef USE_REPORT_PRELUDE
sort = sortBy compare
sortBy cmp = foldr (insertBy cmp) []
#else
sort = sortBy compare
sortBy cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]
    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs
    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = as [a]: sequences bs
    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)
    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs
    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as
#endif /* USE_REPORT_PRELUDE */
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn f =
  map snd . sortBy (comparing fst) . map (\x -> let y = f x in y `seq` (y, x))
unfoldr :: (b -> Maybe (a, b)) -> b -> [a]
{-# INLINE unfoldr #-} 
unfoldr f b0 = build (\c n ->
  let go b = case f b of
               Just (a, new_b) -> a `c` go new_b
               Nothing         -> n
  in go b0)
lines                   :: String -> [String]
lines ""                =  []
lines s                 =  cons (case break (== '\n') s of
                                    (l, s') -> (l, case s' of
                                                    []      -> []
                                                    _:s''   -> lines s''))
  where
    cons ~(h, t)        =  h : t
unlines                 :: [String] -> String
#ifdef USE_REPORT_PRELUDE
unlines                 =  concatMap (++ "\n")
#else
unlines [] = []
unlines (l:ls) = l ++ '\n' : unlines ls
#endif
words                   :: String -> [String]
{-# NOINLINE [1] words #-}
words s                 =  case dropWhile isSpace s of
                                "" -> []
                                s' -> w : words s''
                                      where (w, s'') =
                                             break isSpace s'
{-# RULES
"words" [~1] forall s . words s = build (\c n -> wordsFB c n s)
"wordsList" [1] wordsFB (:) [] = words
 #-}
wordsFB :: ([Char] -> b -> b) -> b -> String -> b
{-# NOINLINE [0] wordsFB #-}
wordsFB c n = go
  where
    go s = case dropWhile isSpace s of
             "" -> n
             s' -> w `c` go s''
                   where (w, s'') = break isSpace s'
unwords                 :: [String] -> String
#ifdef USE_REPORT_PRELUDE
unwords []              =  ""
unwords ws              =  foldr1 (\w s -> w ++ ' ':s) ws
#else
{-# NOINLINE [1] unwords #-}
unwords []              =  ""
unwords (w:ws)          = w ++ go ws
  where
    go []     = ""
    go (v:vs) = ' ' : (v ++ go vs)
{-# RULES
"unwords" [~1] forall ws .
   unwords ws = tailUnwords (foldr unwordsFB "" ws)
"unwordsList" [1] forall ws .
   tailUnwords (foldr unwordsFB "" ws) = unwords ws
 #-}
{-# INLINE [0] tailUnwords #-}
tailUnwords           :: String -> String
tailUnwords []        = []
tailUnwords (_:xs)    = xs
{-# INLINE [0] unwordsFB #-}
unwordsFB               :: String -> String -> String
unwordsFB w r           = ' ' : w ++ r
#endif
data SnocBuilder a = SnocBuilder {-# UNPACK #-} !Word [a] [a]
{-# INLINE sb #-}
sb :: Word -> [a] -> [a] -> SnocBuilder a
sb lp f r
  | lp < 255 || (lp .&. (lp + 1)) /= 0 = SnocBuilder lp f r
  | otherwise                          = SnocBuilder lp (f ++ reverse r) []
emptySB :: SnocBuilder a
emptySB = SnocBuilder 0 [] []
snocSB :: SnocBuilder a -> a -> SnocBuilder a
snocSB (SnocBuilder lp f r) x = sb (lp + 1) f (x:r)
toListSB :: SnocBuilder a -> [a]
toListSB (SnocBuilder _ f r) = f ++ reverse r
