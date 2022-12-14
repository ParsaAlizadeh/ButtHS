{-# LANGUAGE OverloadedStrings #-}

module Echobot ( dispatcher ) where

import Control.Monad

import ButtHS.TgConv


echoConv :: Handler
echoConv = do
  (_,t) <- awaitText
  void $ sendTextM t
  echoConv

dispatcher :: Dispatcher
dispatcher = fallbackD echoConv
