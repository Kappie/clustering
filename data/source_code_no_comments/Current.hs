{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE CPP
           , MagicHash
           , UnboxedTuples
           , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Control.Concurrent (
        ThreadId,
        myThreadId,
        forkIO,
        forkFinally,
        forkIOWithUnmask,
        killThread,
        throwTo,
        forkOn,
        forkOnWithUnmask,
        getNumCapabilities,
        setNumCapabilities,
        threadCapability,
        yield,
        threadDelay,
        threadWaitRead,
        threadWaitWrite,
        threadWaitReadSTM,
        threadWaitWriteSTM,
        module Control.Concurrent.MVar,
        module Control.Concurrent.Chan,
        module Control.Concurrent.QSem,
        module Control.Concurrent.QSemN,
        rtsSupportsBoundThreads,
        forkOS,
        isCurrentThreadBound,
        runInBoundThread,
        runInUnboundThread,
        mkWeakThreadId,
    ) where
import Control.Exception.Base as Exception
import GHC.Conc hiding (threadWaitRead, threadWaitWrite,
                        threadWaitReadSTM, threadWaitWriteSTM)
import GHC.IO           ( unsafeUnmask )
import GHC.IORef        ( newIORef, readIORef, writeIORef )
import GHC.Base
import System.Posix.Types ( Fd )
import Foreign.StablePtr
import Foreign.C.Types
#ifdef mingw32_HOST_OS
import Foreign.C
import System.IO
import Data.Functor ( void )
#else
import qualified GHC.Conc
#endif
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Concurrent.QSem
import Control.Concurrent.QSemN
forkFinally :: IO a -> (Either SomeException a -> IO ()) -> IO ThreadId
forkFinally action and_then =
  mask $ \restore ->
    forkIO $ try (restore action) >>= and_then
foreign import ccall rtsSupportsBoundThreads :: Bool
forkOS :: IO () -> IO ThreadId
foreign export ccall forkOS_entry
    :: StablePtr (IO ()) -> IO ()
foreign import ccall "forkOS_entry" forkOS_entry_reimported
    :: StablePtr (IO ()) -> IO ()
forkOS_entry :: StablePtr (IO ()) -> IO ()
forkOS_entry stableAction = do
        action <- deRefStablePtr stableAction
        action
foreign import ccall forkOS_createThread
    :: StablePtr (IO ()) -> IO CInt
failNonThreaded :: IO a
failNonThreaded = fail $ "RTS doesn't support multiple OS threads "
                       ++"(use ghc -threaded when linking)"
forkOS action0
    | rtsSupportsBoundThreads = do
        mv <- newEmptyMVar
        b <- Exception.getMaskingState
        let
            action1 = case b of
                        Unmasked -> unsafeUnmask action0
                        MaskedInterruptible -> action0
                        MaskedUninterruptible -> uninterruptibleMask_ action0
            action_plus = Exception.catch action1 childHandler
        entry <- newStablePtr (myThreadId >>= putMVar mv >> action_plus)
        err <- forkOS_createThread entry
        when (err /= 0) $ fail "Cannot create OS thread."
        tid <- takeMVar mv
        freeStablePtr entry
        return tid
    | otherwise = failNonThreaded
isCurrentThreadBound :: IO Bool
isCurrentThreadBound = IO $ \ s# ->
    case isCurrentThreadBound# s# of
        (# s2#, flg #) -> (# s2#, isTrue# (flg /=# 0#) #)
runInBoundThread :: IO a -> IO a
runInBoundThread action
    | rtsSupportsBoundThreads = do
        bound <- isCurrentThreadBound
        if bound
            then action
            else do
                ref <- newIORef undefined
                let action_plus = Exception.try action >>= writeIORef ref
                bracket (newStablePtr action_plus)
                        freeStablePtr
                        (\cEntry -> forkOS_entry_reimported cEntry >> readIORef ref) >>=
                  unsafeResult
    | otherwise = failNonThreaded
runInUnboundThread :: IO a -> IO a
runInUnboundThread action = do
  bound <- isCurrentThreadBound
  if bound
    then do
      mv <- newEmptyMVar
      mask $ \restore -> do
        tid <- forkIO $ Exception.try (restore action) >>= putMVar mv
        let wait = takeMVar mv `Exception.catch` \(e :: SomeException) ->
                     Exception.throwTo tid e >> wait
        wait >>= unsafeResult
    else action
unsafeResult :: Either SomeException a -> IO a
unsafeResult = either Exception.throwIO return
threadWaitRead :: Fd -> IO ()
threadWaitRead fd
#ifdef mingw32_HOST_OS
  | threaded  = withThread (waitFd fd 0)
  | otherwise = case fd of
                  0 -> do _ <- hWaitForInput stdin (-1)
                          return ()
                  _ -> error "threadWaitRead requires -threaded on Windows, or use System.IO.hWaitForInput"
#else
  = GHC.Conc.threadWaitRead fd
#endif
threadWaitWrite :: Fd -> IO ()
threadWaitWrite fd
#ifdef mingw32_HOST_OS
  | threaded  = withThread (waitFd fd 1)
  | otherwise = error "threadWaitWrite requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitWrite fd
#endif
threadWaitReadSTM :: Fd -> IO (STM (), IO ())
threadWaitReadSTM fd
#ifdef mingw32_HOST_OS
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do result <- try (waitFd fd 0)
                                             atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = error "threadWaitReadSTM requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitReadSTM fd
#endif
threadWaitWriteSTM :: Fd -> IO (STM (), IO ())
threadWaitWriteSTM fd
#ifdef mingw32_HOST_OS
  | threaded = do v <- newTVarIO Nothing
                  mask_ $ void $ forkIO $ do result <- try (waitFd fd 1)
                                             atomically (writeTVar v $ Just result)
                  let waitAction = do result <- readTVar v
                                      case result of
                                        Nothing         -> retry
                                        Just (Right ()) -> return ()
                                        Just (Left e)   -> throwSTM (e :: IOException)
                  let killAction = return ()
                  return (waitAction, killAction)
  | otherwise = error "threadWaitWriteSTM requires -threaded on Windows"
#else
  = GHC.Conc.threadWaitWriteSTM fd
#endif
#ifdef mingw32_HOST_OS
foreign import ccall unsafe "rtsSupportsBoundThreads" threaded :: Bool
withThread :: IO a -> IO a
withThread io = do
  m <- newEmptyMVar
  _ <- mask_ $ forkIO $ try io >>= putMVar m
  x <- takeMVar m
  case x of
    Right a -> return a
    Left e  -> throwIO (e :: IOException)
waitFd :: Fd -> CInt -> IO ()
waitFd fd write = do
   throwErrnoIfMinus1_ "fdReady" $
        fdReady (fromIntegral fd) write iNFINITE 0
iNFINITE :: CInt
iNFINITE = 0xFFFFFFFF 
foreign import ccall safe "fdReady"
  fdReady :: CInt -> CInt -> CInt -> CInt -> IO CInt
#endif
