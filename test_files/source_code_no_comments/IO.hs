{-# LANGUAGE Unsafe #-}
{-# LANGUAGE NoImplicitPrelude
           , BangPatterns
           , RankNTypes
           , MagicHash
           , UnboxedTuples
  #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# OPTIONS_HADDOCK hide #-}
module GHC.IO (
        IO(..), unIO, failIO, liftIO,
        unsafePerformIO, unsafeInterleaveIO,
        unsafeDupablePerformIO, unsafeDupableInterleaveIO,
        noDuplicate,
        stToIO, ioToST, unsafeIOToST, unsafeSTToIO,
        FilePath,
        catchException, catchAny, throwIO,
        mask, mask_, uninterruptibleMask, uninterruptibleMask_,
        MaskingState(..), getMaskingState,
        unsafeUnmask,
        onException, bracket, finally, evaluate
    ) where
import GHC.Base
import GHC.ST
import GHC.Exception
import GHC.Show
import {-# SOURCE #-} GHC.IO.Exception ( userError )
liftIO :: IO a -> State# RealWorld -> STret RealWorld a
liftIO (IO m) = \s -> case m s of (# s', r #) -> STret s' r
failIO :: String -> IO a
failIO s = IO (raiseIO# (toException (userError s)))
stToIO        :: ST RealWorld a -> IO a
stToIO (ST m) = IO m
ioToST        :: IO a -> ST RealWorld a
ioToST (IO m) = (ST m)
unsafeIOToST        :: IO a -> ST s a
unsafeIOToST (IO io) = ST $ \ s -> (unsafeCoerce# io) s
unsafeSTToIO :: ST s a -> IO a
unsafeSTToIO (ST m) = IO (unsafeCoerce# m)
unsafePerformIO :: IO a -> a
unsafePerformIO m = unsafeDupablePerformIO (noDuplicate >> m)
{-# NOINLINE unsafeDupablePerformIO #-}
unsafeDupablePerformIO  :: IO a -> a
unsafeDupablePerformIO (IO m) = lazy (case m realWorld# of (# _, r #) -> r)
{-# INLINE unsafeInterleaveIO #-}
unsafeInterleaveIO :: IO a -> IO a
unsafeInterleaveIO m = unsafeDupableInterleaveIO (noDuplicate >> m)
{-# NOINLINE unsafeDupableInterleaveIO #-}
unsafeDupableInterleaveIO :: IO a -> IO a
unsafeDupableInterleaveIO (IO m)
  = IO ( \ s -> let
                   r = case m s of (# _, res #) -> res
                in
                (# s, r #))
noDuplicate :: IO ()
noDuplicate = IO $ \s -> case noDuplicate# s of s' -> (# s', () #)
type FilePath = String
catchException :: Exception e => IO a -> (e -> IO a) -> IO a
catchException (IO io) handler = IO $ catch# io handler'
    where handler' e = case fromException e of
                       Just e' -> unIO (handler e')
                       Nothing -> raiseIO# e
catchAny :: IO a -> (forall e . Exception e => e -> IO a) -> IO a
catchAny (IO io) handler = IO $ catch# io handler'
    where handler' (SomeException e) = unIO (handler e)
throwIO :: Exception e => e -> IO a
throwIO e = IO (raiseIO# (toException e))
block :: IO a -> IO a
block (IO io) = IO $ maskAsyncExceptions# io
unblock :: IO a -> IO a
unblock = unsafeUnmask
unsafeUnmask :: IO a -> IO a
unsafeUnmask (IO io) = IO $ unmaskAsyncExceptions# io
blockUninterruptible :: IO a -> IO a
blockUninterruptible (IO io) = IO $ maskUninterruptible# io
data MaskingState
  = Unmasked 
  | MaskedInterruptible
  | MaskedUninterruptible
 deriving (Eq,Show)
getMaskingState :: IO MaskingState
getMaskingState  = IO $ \s ->
  case getMaskingState# s of
     (# s', i #) -> (# s', case i of
                             0# -> Unmasked
                             1# -> MaskedUninterruptible
                             _  -> MaskedInterruptible #)
onException :: IO a -> IO b -> IO a
onException io what = io `catchException` \e -> do _ <- what
                                                   throwIO (e :: SomeException)
mask  :: ((forall a. IO a -> IO a) -> IO b) -> IO b
mask_ :: IO a -> IO a
uninterruptibleMask :: ((forall a. IO a -> IO a) -> IO b) -> IO b
uninterruptibleMask_ :: IO a -> IO a
mask_ io = mask $ \_ -> io
mask io = do
  b <- getMaskingState
  case b of
    Unmasked -> block $ io unblock
    _        -> io id
uninterruptibleMask_ io = uninterruptibleMask $ \_ -> io
uninterruptibleMask io = do
  b <- getMaskingState
  case b of
    Unmasked              -> blockUninterruptible $ io unblock
    MaskedInterruptible   -> blockUninterruptible $ io block
    MaskedUninterruptible -> io id
bracket
        :: IO a         
        -> (a -> IO b)  
        -> (a -> IO c)  
        -> IO c         
bracket before after thing =
  mask $ \restore -> do
    a <- before
    r <- restore (thing a) `onException` after a
    _ <- after a
    return r
finally :: IO a         
        -> IO b         
        -> IO a         
a `finally` sequel =
  mask $ \restore -> do
    r <- restore a `onException` sequel
    _ <- sequel
    return r
evaluate :: a -> IO a
evaluate a = IO $ \s -> seq# a s 
