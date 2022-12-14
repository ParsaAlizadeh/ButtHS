{-# LANGUAGE GADTs #-}

module ButtHS.PipeT
  ( PipeT
  , await
  , stepPipe
  , runPipe
  ) where

import Control.Monad
import Control.Monad.Trans


data PipeT i m a where
  Done        :: a -> PipeT i m a
  NeedInput   :: (i -> PipeT i m a) -> PipeT i m a
  NeedAction  :: m (PipeT i m a) -> PipeT i m a

instance Monad m => Functor (PipeT i m) where
  fmap = liftM

instance Monad m => Applicative (PipeT i m) where
  pure = Done
  (<*>) = ap

instance Monad m => Monad (PipeT i m) where
  return = pure

  Done a >>= p = p a
  NeedInput f >>= p = NeedInput (f >=> p)
  NeedAction act >>= p = NeedAction ((>>= p) <$> act)

instance MonadTrans (PipeT i) where
  lift = NeedAction . (Done <$>)

instance MonadIO m => MonadIO (PipeT i m) where
  liftIO = lift . liftIO

await :: Monad m => PipeT i m i
await = NeedInput return

stepPipe :: Monad m => PipeT i m a -> [i] -> m (Either a (PipeT i m a))
stepPipe   (Done a)         _       = return $ Left a
stepPipe p@(NeedInput _)    []      = return $ Right p
stepPipe   (NeedInput f)    (u:us)  = stepPipe (f u) us
stepPipe   (NeedAction act) u       = act >>= (`stepPipe` u)

runPipe :: Monad m => PipeT i m a -> m [i] -> m a
runPipe p getInputs = do
  us <- getInputs
  r  <- stepPipe p us
  case r of
    Left a    -> return a
    Right p'  -> runPipe p' getInputs
