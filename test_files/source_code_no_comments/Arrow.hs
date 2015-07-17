{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.Arrow (
    Arrow(..), Kleisli(..),
    returnA,
    (^>>), (>>^),
    (>>>), (<<<), 
    (<<^), (^<<),
    ArrowZero(..), ArrowPlus(..),
    ArrowChoice(..),
    ArrowApply(..), ArrowMonad(..), leftApp,
    ArrowLoop(..)
    ) where
import Data.Tuple ( fst, snd, uncurry )
import Data.Either
import Control.Monad.Fix
import Control.Category
import GHC.Base hiding ( (.), id )
infixr 5 <+>
infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||
infixr 1 ^>>, >>^
infixr 1 ^<<, <<^
class Category a => Arrow a where
    arr :: (b -> c) -> a b c
    first :: a b c -> a (b,d) (c,d)
    second :: a b c -> a (d,b) (d,c)
    second f = arr swap >>> first f >>> arr swap
      where
        swap :: (x,y) -> (y,x)
        swap ~(x,y) = (y,x)
    (***) :: a b c -> a b' c' -> a (b,b') (c,c')
    f *** g = first f >>> second g
    (&&&) :: a b c -> a b c' -> a b (c,c')
    f &&& g = arr (\b -> (b,b)) >>> f *** g
{-# RULES
"compose/arr"   forall f g .
                (arr f) . (arr g) = arr (f . g)
"first/arr"     forall f .
                first (arr f) = arr (first f)
"second/arr"    forall f .
                second (arr f) = arr (second f)
"product/arr"   forall f g .
                arr f *** arr g = arr (f *** g)
"fanout/arr"    forall f g .
                arr f &&& arr g = arr (f &&& g)
"compose/first" forall f g .
                (first f) . (first g) = first (f . g)
"compose/second" forall f g .
                (second f) . (second g) = second (f . g)
 #-}
instance Arrow (->) where
    arr f = f
    first f = f *** id
    second f = id *** f
    (***) f g ~(x,y) = (f x, g y)
newtype Kleisli m a b = Kleisli { runKleisli :: a -> m b }
instance Monad m => Category (Kleisli m) where
    id = Kleisli return
    (Kleisli f) . (Kleisli g) = Kleisli (\b -> g b >>= f)
instance Monad m => Arrow (Kleisli m) where
    arr f = Kleisli (return . f)
    first (Kleisli f) = Kleisli (\ ~(b,d) -> f b >>= \c -> return (c,d))
    second (Kleisli f) = Kleisli (\ ~(d,b) -> f b >>= \c -> return (d,c))
returnA :: Arrow a => a b b
returnA = arr id
(^>>) :: Arrow a => (b -> c) -> a c d -> a b d
f ^>> a = arr f >>> a
(>>^) :: Arrow a => a b c -> (c -> d) -> a b d
a >>^ f = a >>> arr f
(<<^) :: Arrow a => a c d -> (b -> c) -> a b d
a <<^ f = a <<< arr f
(^<<) :: Arrow a => (c -> d) -> a b c -> a b d
f ^<< a = arr f <<< a
class Arrow a => ArrowZero a where
    zeroArrow :: a b c
instance MonadPlus m => ArrowZero (Kleisli m) where
    zeroArrow = Kleisli (\_ -> mzero)
class ArrowZero a => ArrowPlus a where
    (<+>) :: a b c -> a b c -> a b c
instance MonadPlus m => ArrowPlus (Kleisli m) where
    Kleisli f <+> Kleisli g = Kleisli (\x -> f x `mplus` g x)
class Arrow a => ArrowChoice a where
    left :: a b c -> a (Either b d) (Either c d)
    right :: a b c -> a (Either d b) (Either d c)
    right f = arr mirror >>> left f >>> arr mirror
      where
        mirror :: Either x y -> Either y x
        mirror (Left x) = Right x
        mirror (Right y) = Left y
    (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
    f +++ g = left f >>> right g
    (|||) :: a b d -> a c d -> a (Either b c) d
    f ||| g = f +++ g >>> arr untag
      where
        untag (Left x) = x
        untag (Right y) = y
{-# RULES
"left/arr"      forall f .
                left (arr f) = arr (left f)
"right/arr"     forall f .
                right (arr f) = arr (right f)
"sum/arr"       forall f g .
                arr f +++ arr g = arr (f +++ g)
"fanin/arr"     forall f g .
                arr f ||| arr g = arr (f ||| g)
"compose/left"  forall f g .
                left f . left g = left (f . g)
"compose/right" forall f g .
                right f . right g = right (f . g)
 #-}
instance ArrowChoice (->) where
    left f = f +++ id
    right f = id +++ f
    f +++ g = (Left . f) ||| (Right . g)
    (|||) = either
instance Monad m => ArrowChoice (Kleisli m) where
    left f = f +++ arr id
    right f = arr id +++ f
    f +++ g = (f >>> arr Left) ||| (g >>> arr Right)
    Kleisli f ||| Kleisli g = Kleisli (either f g)
class Arrow a => ArrowApply a where
    app :: a (a b c, b) c
instance ArrowApply (->) where
    app (f,x) = f x
instance Monad m => ArrowApply (Kleisli m) where
    app = Kleisli (\(Kleisli f, x) -> f x)
newtype ArrowMonad a b = ArrowMonad (a () b)
instance Arrow a => Functor (ArrowMonad a) where
    fmap f (ArrowMonad m) = ArrowMonad $ m >>> arr f
instance Arrow a => Applicative (ArrowMonad a) where
   pure x = ArrowMonad (arr (const x))
   ArrowMonad f <*> ArrowMonad x = ArrowMonad (f &&& x >>> arr (uncurry id))
instance ArrowApply a => Monad (ArrowMonad a) where
    return x = ArrowMonad (arr (\_ -> x))
    ArrowMonad m >>= f = ArrowMonad $
        m >>> arr (\x -> let ArrowMonad h = f x in (h, ())) >>> app
instance ArrowPlus a => Alternative (ArrowMonad a) where
   empty = ArrowMonad zeroArrow
   ArrowMonad x <|> ArrowMonad y = ArrowMonad (x <+> y)
instance (ArrowApply a, ArrowPlus a) => MonadPlus (ArrowMonad a) where
   mzero = ArrowMonad zeroArrow
   ArrowMonad x `mplus` ArrowMonad y = ArrowMonad (x <+> y)
leftApp :: ArrowApply a => a b c -> a (Either b d) (Either c d)
leftApp f = arr ((\b -> (arr (\() -> b) >>> f >>> arr Left, ())) |||
             (\d -> (arr (\() -> d) >>> arr Right, ()))) >>> app
class Arrow a => ArrowLoop a where
    loop :: a (b,d) (c,d) -> a b c
instance ArrowLoop (->) where
    loop f b = let (c,d) = f (b,d) in c
instance MonadFix m => ArrowLoop (Kleisli m) where
    loop (Kleisli f) = Kleisli (liftM fst . mfix . f')
      where f' x y = f (x, snd y)
