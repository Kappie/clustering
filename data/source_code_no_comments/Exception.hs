{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, ExistentialQuantification #-}
module Control.Exception (
        SomeException(..),
        Exception(..),          
        IOException,            
        ArithException(..),     
        ArrayException(..),     
        AssertionFailed(..),
        SomeAsyncException(..),
        AsyncException(..),     
        asyncExceptionToException, asyncExceptionFromException,
        NonTermination(..),
        NestedAtomically(..),
        BlockedIndefinitelyOnMVar(..),
        BlockedIndefinitelyOnSTM(..),
        AllocationLimitExceeded(..),
        Deadlock(..),
        NoMethodError(..),
        PatternMatchFail(..),
        RecConError(..),
        RecSelError(..),
        RecUpdError(..),
        ErrorCall(..),
        throw,
        throwIO,
        ioError,
        throwTo,
        catch,
        catches, Handler(..),
        catchJust,
        handle,
        handleJust,
        try,
        tryJust,
        evaluate,
        mapException,
        mask,
        mask_,
        uninterruptibleMask,
        uninterruptibleMask_,
        MaskingState(..),
        getMaskingState,
        allowInterrupt,
        assert,
        bracket,
        bracket_,
        bracketOnError,
        finally,
        onException,
  ) where
import Control.Exception.Base
import GHC.Base
import GHC.IO (unsafeUnmask)
data Handler a = forall e . Exception e => Handler (e -> IO a)
instance Functor Handler where
     fmap f (Handler h) = Handler (fmap f . h)
catches :: IO a -> [Handler a] -> IO a
catches io handlers = io `catch` catchesHandler handlers
catchesHandler :: [Handler a] -> SomeException -> IO a
catchesHandler handlers e = foldr tryHandler (throw e) handlers
    where tryHandler (Handler handler) res
              = case fromException e of
                Just e' -> handler e'
                Nothing -> res
allowInterrupt :: IO ()
allowInterrupt = unsafeUnmask $ return ()
