{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module RockPaperScissors ( dispatcher ) where

import Control.Monad
import System.Random

import ButtHS.TgConv

import qualified Data.Text as T


data Choice = Rock | Paper | Scissors deriving (Show, Enum, Eq)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

awaitChoice :: TgConv (MessageId, Choice)
awaitChoice = do
  (i,inp) <- awaitText
  let txt = T.toLower $ T.strip $ inp
  if | txt == "rock"      -> return (i, Rock)
     | txt == "paper"     -> return (i, Paper)
     | txt == "scissors"  -> return (i, Scissors)
     | otherwise          -> do
          void $ replyTextM i "please send one of rock, paper or scissors"
          awaitChoice

awaitRange :: Int -> Int -> TgConv (MessageId, Int)
awaitRange l r = do
  (i,num) <- awaitNumber
  if l <= num && num <= r
    then return (i,num)
    else do
      void $ replyTextM i $ "please send a number between " <> tshow l <> " and " <> tshow r
      awaitRange l r

gameScore :: Choice -> Choice -> Int
gameScore Rock Scissors = 1
gameScore Scissors Paper = 1
gameScore Paper Rock = 1
gameScore x y
  | x == y = 0
  | otherwise = -1

aiChoice :: TgConv Choice
aiChoice = toEnum <$> getStdRandom (randomR (0,2))

gameConv :: TgConv Int
gameConv = do
  void $ sendTextM "rock, paper, scissor"
  (i,player) <- awaitChoice
  ai <- aiChoice
  void $ replyTextM i $ "ai played " <> T.toLower (tshow ai)
  let score = gameScore player ai
  void $ sendTextM $
    if | score > 0 -> "+1 point for you"
       | score < 0 -> "+1 point for AI"
       | otherwise -> "same"
  return score

startConv :: Handler
startConv = do
  (i,_) <- awaitText
  void $ replyTextM i "welcome to rps. send me number of rounds to start the game (between 1 and 5)"
  (i,cnt) <- awaitRange 1 5
  void $ replyTextM i $ "ok, lets play for " <> tshow cnt <> " rounds"
  scores <- replicateM cnt gameConv
  let playerScore = length $ filter (> 0) scores
      aiScore = length $ filter (< 0) scores
  void $ sendTextM $ "you: " <> tshow playerScore <> " points\nai: " <> tshow aiScore <> " points"
  void $ sendTextM $
    if | playerScore > aiScore -> "you win :("
       | playerScore < aiScore -> "you can't defeat AI XD"
       | otherwise             -> "DRAW"

usageConv :: Handler
usageConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "/start the game!"

dispatcher :: Dispatcher
dispatcher = commandD "start" startConv
          <> fallbackD usageConv
