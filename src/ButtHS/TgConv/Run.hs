module ButtHS.TgConv.Run
  ( runTgPipe
  , runTgConv
  , runButt'
  , runButt
  , prefixD
  , commandD
  , fallbackD
  ) where

import Control.Monad.Reader

import ButtHS.TgConv.Types
import ButtHS.TgPipe

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as W

import qualified ButtHS.Butt as Butt


runTgConv :: TgConv a -> W.ChatId -> TgPipe a
runTgConv = runReaderT

mkDispather :: Dispatcher -> Butt.Dispatcher
mkDispather (Dispatcher dispatcher) upd
  | Just msg <- W.message upd =
      let cid = W.chat_id $ W.chat msg
      in  pure $ (`runTgConv` W.ChatId cid) <$> dispatcher msg
  | otherwise = pure Nothing

runButt' :: Dispatcher -> TgPipe ()
runButt' = Butt.runButt . mkDispather

runButt :: Dispatcher -> W.TelegramClient ()
runButt = runTgPipe . runButt'

prefixD :: T.Text -> Handler -> Dispatcher
prefixD cmd h = Dispatcher dispatcher where
  dispatcher msg = do
    t <- W.text msg
    if (== cmd) $ T.takeWhile (/= ' ') t
      then return h
      else Nothing

commandD :: T.Text -> Handler -> Dispatcher
commandD = prefixD . T.cons '/'

fallbackD :: Handler -> Dispatcher
fallbackD h = Dispatcher $ const $ Just h