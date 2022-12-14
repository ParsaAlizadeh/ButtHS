module ButtHS.TgConv.Action
  ( -- Awaits
    awaitUpdate
  , awaitMessage
  , textOfMessage
  , awaitText
  , awaitNumber
    -- Yields
  , sendMessageM
  , sendMessageRequest
  , sendTextM
  , replyTextM
  ) where

import Control.Monad.Reader

import ButtHS.TgConv.Types
import ButtHS.TgPipe

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Web.Telegram.API.Bot as W


awaitUpdate :: TgConv W.Update
awaitUpdate = lift await

awaitMessage :: TgConv W.Message
awaitMessage = do
  u <- awaitUpdate
  case W.message u of
    Just msg -> return msg
    -- Nothing is impossible

textOfMessage :: W.Message -> T.Text
textOfMessage = maybe T.empty id . W.text

awaitText :: TgConv (MessageId, T.Text)
awaitText = do
  msg <- awaitMessage
  let text = textOfMessage msg
  if T.null text
    then awaitText
    else return (W.message_id msg, text)

awaitNumber :: Integral a => TgConv (MessageId, a)
awaitNumber = do
  (id,txt) <- awaitText
  case T.signed T.decimal txt of
    Left _ -> awaitNumber
    Right (a,rest) ->
      if T.null rest
        then return (id,a)
        else awaitNumber

sendMessageM :: W.SendMessageRequest -> TgConv W.MessageResponse
sendMessageM r = do
  chatId <- ask
  liftTg $ W.sendMessageM (r { W.message_chat_id = chatId })

sendMessageRequest :: T.Text -> W.SendMessageRequest
sendMessageRequest = W.sendMessageRequest (W.ChatId 0)

sendTextM :: T.Text -> TgConv W.MessageResponse
sendTextM = sendMessageM . sendMessageRequest

replyTextM :: MessageId -> T.Text -> TgConv W.MessageResponse
replyTextM mid text = sendMessageM $ (sendMessageRequest text)
  { W.message_reply_to_message_id = Just mid }