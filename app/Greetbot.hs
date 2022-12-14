{-# LANGUAGE OverloadedStrings #-}

module Greetbot ( dispatcher ) where

import Control.Monad

import ButtHS.TgConv


greetConv :: Handler
greetConv = do
  (id,_) <- awaitText
  void $ replyTextM id "hello! what is your name?"
  (id,name) <- awaitText
  void $ replyTextM id $ "nice to meet you " <> name <> "!\nwhere are you from?"
  (id,country) <- awaitText
  void $ replyTextM id $ "so you are " <> name <> " from " <> country <> "."
  void $ sendTextM "have a nice day!"

helpConv :: Handler
helpConv = do
  (id,_) <- awaitText
  void $ replyTextM id $ "/start to start the bot\n/help to see the list of commands"

fallbackConv :: Handler
fallbackConv = do
  (id,_) <- awaitText
  void $ replyTextM id $ "unable to understand, use /help to see the list of commands"

dispatcher :: Dispatcher
dispatcher = commandD "start" greetConv
          <> commandD "help" helpConv
          <> fallbackD fallbackConv