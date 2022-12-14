{-# LANGUAGE UndecidableInstances #-}

module ButtHS.TgPipe
  ( TgPipe
  , liftTg
  , runTgPipe
  , await
  , stepPipe
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.State.Strict

import ButtHS.PipeT

import qualified Web.Telegram.API.Bot as W


type TgPipe = PipeT W.Update W.TelegramClient

class Monad m => MonadTg m where
  liftTg :: W.TelegramClient a -> m a

instance MonadTg W.TelegramClient where
  liftTg = id

instance {-# INCOHERENT #-} (MonadTg m, MonadTrans t, Monad (t m)) => MonadTg (t m) where
  liftTg = lift . liftTg

getUpdatesM :: StateT Int W.TelegramClient [W.Update]
getUpdatesM = do
  offset <- get
  W.Response { W.result = us } <- lift $ W.getUpdatesM $ (W.getUpdatesRequest)
    { W.updates_offset  = Just offset
    , W.updates_timeout = Just 1
    }
  liftIO $ print us
  let newOffset = (+ 1) $ maximum $ (offset-1:) $ map W.update_id us
  put newOffset
  return us

runTgPipe :: TgPipe a -> W.TelegramClient a
runTgPipe p = go p `evalStateT` 0 where
  go p = do
    us <- getUpdatesM
    r  <- lift $ stepPipe p us
    liftIO $ threadDelay (500 * 1000) -- microseconds
    case r of
      Left a    -> return a
      Right p'  -> go p'

