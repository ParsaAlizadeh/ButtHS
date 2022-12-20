{-# LANGUAGE OverloadedStrings #-}

module Greetbot ( dispatcher ) where

import Control.Monad

import ButtHS.TgConv


greetConv :: Handler
greetConv = do
  (i,_) <- awaitText
  void $ replyTextM i "hello! what is your name?"
  (i,name) <- awaitText
  void $ replyTextM i $ "nice to meet you " <> name <> "!\nwhere are you from?"
  (i,country) <- awaitText
  void $ replyTextM i $ "so you are " <> name <> " from " <> country <> "."
  void $ sendTextM "have a nice day!"

helpConv :: Handler
helpConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "/start to start the bot\n/help to see the list of commands"

fallbackConv :: Handler
fallbackConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "unable to understand, use /help to see the list of commands"

dispatcher :: Dispatcher
dispatcher = commandD "start" greetConv
          <> commandD "help" helpConv
          <> fallbackD fallbackConv