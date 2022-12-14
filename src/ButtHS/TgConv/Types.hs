module ButtHS.TgConv.Types
  ( TgConv
  , Handler
  , Dispatcher(..)
  , MessageId
  ) where

import Control.Monad.Reader

import ButtHS.TgPipe

import qualified Web.Telegram.API.Bot as W

type TgConv = ReaderT W.ChatId TgPipe
type Handler = TgConv ()
newtype Dispatcher = Dispatcher { unDispacther :: W.Message -> Maybe Handler }

type MessageId = Int

instance Semigroup Dispatcher where
  Dispatcher d1 <> Dispatcher d2 =
    Dispatcher (\msg -> case d1 msg of
      Nothing -> d2 msg
      Just h  -> Just h)

instance Monoid Dispatcher where
  mempty = Dispatcher (const Nothing)
  mappend = (<>)