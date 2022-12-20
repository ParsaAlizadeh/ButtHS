{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}

module XO ( dispatcher ) where

import Control.Monad

import ButtHS.TgConv

import qualified Data.Text as T
import qualified Web.Telegram.API.Bot as W


data Cell = X | O | E deriving (Eq, Enum)
type Board = [[Cell]]

instance Show Cell where
  show X = "X"
  show O = "O"
  show E = "."

instance Semigroup Cell where
  X <> _ = X
  O <> _ = O
  E <> c = c

instance Monoid Cell where
  mempty = E
  mappend = (<>)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

sendBoard :: Board -> TgConv ()
sendBoard board = void $ sendMessageM $ (sendMessageRequest text)
  { W.message_parse_mode = Just W.Markdown } where
    text = T.unlines
      [ "```"
      , "+ 0 1 2"
      , row 0, row 1, row 2
      , "```"
      ]
    row i = tshow i <> " " <> (T.unwords $ map tshow (board!!i))

awaitRange :: Int -> Int -> TgConv (MessageId, Int)
awaitRange l r = do
  (i,num) <- awaitNumber
  if l <= num && num <= r
    then return (i,num)
    else do
      void $ replyTextM i $ "please send a number between " <> tshow l <> " and " <> tshow r
      awaitRange l r

awaitMove :: Board -> TgConv (Int,Int)
awaitMove board = do
  void $ sendTextM "select row (between 0 and 2)"
  (_,r) <- awaitRange 0 2
  void $ sendTextM "select column (between 0 and 2)"
  (_,c) <- awaitRange 0 2
  case board!!r!!c of
    E -> return (r,c)
    _ -> do
      void $ sendTextM "that cell is not empty"
      awaitMove board

checkTripleWinner :: [Cell] -> Cell
checkTripleWinner []  = E
checkTripleWinner [x] = x
checkTripleWinner (x1:x2:xs)
  | x1 == x2  = checkTripleWinner (x2:xs)
  | otherwise = E

checkWinner :: Board -> Cell
checkWinner board = mconcat $ map checkTripleWinner
  [ board!!0
  , board!!1
  , board!!2
  , map (!!0) board
  , map (!!1) board
  , map (!!2) board
  , [board!!0!!0, board!!1!!1, board!!2!!2]
  , [board!!0!!2, board!!1!!1, board!!2!!0]
  ]

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ []     = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt n f (x:xs) = x : modifyAt (n-1) f xs

changeCell :: Int -> Int -> Cell -> Board -> Board
changeCell r c = modifyAt r . modifyAt c . const

opponent :: Cell -> Cell
opponent X = O
opponent O = X
opponent _ = E

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 E

fullBoard :: Board -> Bool
fullBoard = null . filter (== E) . join

gameConv :: Cell -> Board -> TgConv Cell
gameConv player board
  | fullBoard board = return E
  | otherwise = do
      sendBoard board
      void $ sendTextM $ "current turn: " <> tshow player
      (r,c) <- awaitMove board
      let newBoard = changeCell r c player board
      case checkWinner newBoard of
        E -> gameConv (opponent player) newBoard
        winner -> sendBoard newBoard >> return winner

startConv :: Handler
startConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "lets start the game!"
  winner <- gameConv X emptyBoard
  void $ sendTextM $ case winner of
    E -> "DRAW"
    _ -> tshow winner <> " WINS!"

usageConv :: Handler
usageConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "/start the game!"

dispatcher :: Dispatcher
dispatcher = commandD "start" startConv
          <> fallbackD usageConv
