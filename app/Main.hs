{-# LANGUAGE OverloadedStrings #-}

module Main ( main ) where

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import System.Environment (getEnv)

import ButtHS.TgConv (runButt)
import PortalMaze (dispatcher)

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as W


main :: IO ()
main = do
  token <- W.Token . T.pack . ("bot" <>) <$> getEnv "BOT_TOKEN"
  manager <- newManager tlsManagerSettings
  result <- W.runTelegramClient token manager $ runButt dispatcher
  print result
  putStrLn "done!"
