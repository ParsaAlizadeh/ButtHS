module ButtHS.Butt
  ( runButt
  , Handler
  , Butt
  , Dispatcher
  ) where

import Control.Monad.State.Strict
import Data.Int (Int64)

import ButtHS.TgPipe

import qualified Data.HashMap.Strict as H
import qualified Web.Telegram.API.Bot as W


type Handler    = TgPipe ()
type Butt       = StateT (H.HashMap Int64 Handler) TgPipe
type Dispatcher = W.Update -> Butt (Maybe Handler)

justSecond :: Either a b -> Maybe b
justSecond (Right b)  = Just b
justSecond _          = Nothing

alterM :: Dispatcher -> W.Update -> Maybe Handler -> Butt (Maybe Handler)
alterM _          upd (Just p) = fmap justSecond $ liftTg $ stepPipe p [upd]
alterM dispatcher upd Nothing  = do
  mh <- dispatcher upd
  case mh of
    Nothing -> return Nothing
    Just p  -> alterM dispatcher upd (Just p)

modifyM_ :: MonadState s m => (s -> m s) -> m ()
modifyM_ f = get >>= f >>= put

runButt :: Dispatcher -> TgPipe ()
runButt dispatcher = loop `evalStateT` H.empty where
  loop = do
    upd <- lift await
    case W.chat_id . W.chat <$> W.message upd of
      Nothing   -> return ()
      Just cid  -> modifyM_ $ H.alterF (alterM dispatcher upd) cid
    loop
