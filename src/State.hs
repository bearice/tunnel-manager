{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module State where
  import Control.Applicative
  import Control.Concurrent.STM
  import Control.Monad.Reader
  import Data.Default.Class
  import Data.Word
  import Data.IP

  data AppState = AppState {
      tickCount :: Int,
      nextAddr :: Word32
    }

  instance Default AppState where
      def = AppState 0 $ (byteSwap32.toHostAddress) (read "10.16.0.1")

  newtype WebM a = WebM { runWebM :: ReaderT (TVar AppState) IO a }
      deriving (Applicative, Functor, Monad, MonadIO, MonadReader (TVar AppState))

  mkState :: IO (TVar AppState)
  mkState = newTVarIO def

  withState :: TVar AppState -> WebM a -> IO a
  withState s m = runReaderT (runWebM m) s

  -- Some helpers to make this feel more like a state monad.
  webM :: MonadTrans t => WebM a -> t WebM a
  webM = lift

  gets :: (AppState -> b) -> WebM b
  gets f = liftM f (ask >>= liftIO . readTVarIO)

  modify :: (AppState -> AppState) -> WebM ()
  modify f = ask >>= liftIO.atomically.flip modifyTVar' f

  modifyAndGet :: (AppState -> AppState) -> WebM AppState
  modifyAndGet f = ask >>= liftIO.atomically.fn where
    fn x = do
      modifyTVar' x f
      readTVar x

  incrAndGetNextAddr :: MonadTrans t => t WebM IPv4
  incrAndGetNextAddr =
    lift $ do
      next <- modifyAndGet $ \st-> st{ nextAddr = nextAddr st + 1 }
      return $ (fromHostAddress.byteSwap32) (nextAddr next)
