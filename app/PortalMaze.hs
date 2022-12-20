{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}

module PortalMaze ( dispatcher ) where

import Control.Monad
import Control.Monad.Cont
import Control.Monad.State.Strict
import Data.Char

import ButtHS.TgConv

import qualified Data.Text as T
import qualified Data.IntMap.Strict as I
import qualified Web.Telegram.API.Bot as W

newtype CoroutineT r m a = CoroutineT
  { unCoroutineT :: ContT r (StateT (I.IntMap (CoroutineT r m ())) m) a }
  deriving (Functor, Applicative, Monad, MonadCont, MonadIO)

instance MonadTrans (CoroutineT r) where
  lift = CoroutineT . lift . lift

instance Monad m => MonadState (I.IntMap (CoroutineT r m ())) (CoroutineT r m) where
  get = CoroutineT $ lift get
  put = CoroutineT . lift . put

runCoroutineT :: Monad m => CoroutineT r m r -> m r
runCoroutineT = flip evalStateT I.empty . flip runContT return . unCoroutineT

type MazeConv r = CoroutineT r TgConv

gameBoard :: [String]
gameBoard =
  [ "#########"
  , "#.#....1#"
  , "#0..###.#"
  , "#.##4...#"
  , "#..2#..##"
  , "###..#..#"
  , "#...#..##"
  , "#3.#..5G#"
  , "#########"
  ]

startingPoint :: (Int,Int)
startingPoint = (1,1)

boardWidth :: Int
boardWidth = length gameBoard

boardView :: (Int,Int) -> [String]
boardView (i,j) = [ take 3 $ drop (j-1) $ gameBoard!!x | x <- [i-1..i+1] ]

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt _ _ [] = []
modifyAt 0 f (x:xs) = f x : xs
modifyAt n f (x:xs) = x : modifyAt (n-1) f xs

toDigit :: Char -> Maybe Int
toDigit c
  | isDigit c = Just $ digitToInt c
  | otherwise = Nothing

sendPanelView :: (Int,Int) -> MazeConv r ()
sendPanelView pos = do
  keys <- T.pack . show . I.keys <$> get
  let txt = T.unlines $ ["```"] <> body <> ["", keys, "```"]
      body = map (T.center 7 ' ') $
        [ "W"
        , T.pack $ currentView!!0
        , "A " <> (T.pack $ currentView!!1) <> " D"
        , T.pack $ currentView!!2
        , "S"
        ]
      currentView = boardView pos
  void $ lift $ sendMessageM $ (sendMessageRequest txt)
    { W.message_parse_mode = Just W.Markdown }

moveBy :: Char -> (Int,Int) -> Maybe (Int,Int)
moveBy 'W' (i,j) = Just (i-1,j)
moveBy 'A' (i,j) = Just (i,j-1)
moveBy 'S' (i,j) = Just (i+1,j)
moveBy 'D' (i,j) = Just (i,j+1)
moveBy c p
  | isLower c = moveBy (toUpper c) p
  | otherwise = Nothing

getAt :: (Int,Int) -> Char
getAt (i,j) = gameBoard!!i!!j

awaitMove :: MazeConv r (Either Int Char) -> (Int,Int) -> MazeConv r (Either Int Char)
awaitMove cancel pos = do
  (i,m) <- lift $ awaitText
  if | m == "/cancel" -> do
        void $ lift $ replyTextM i "ok, end the game"
        cancel
     | T.length m /= 1 -> do
        void $ lift $ replyTextM i "enter one of WASD, a digit, or /cancel"
        awaitMove cancel pos
     | otherwise -> do
        let c = T.head m
        if | Just d <- toDigit c -> do
              return $ Left d
           | Just _ <- moveBy c pos -> do
              return $ Right c
           | otherwise -> do
              void $ lift $ replyTextM i "enter one of WASD, a digit, or /cancel"
              awaitMove cancel pos

gameConv :: (Int,Int) -> MazeConv r ()
gameConv pos = callCC $ \cancel -> gameConv' (cancel ()) pos where
  gameConv' cancel pos = do
    sendPanelView pos
    move <- awaitMove cancel pos
    case move of
      Left d -> do
        maybekill <- gets (I.lookup d)
        case maybekill of
          Nothing -> do
            void $ lift $ sendTextM "this checkpoint is not unlocked"
            gameConv' cancel pos
          Just kill -> do
            kill
      Right c -> do
        let Just newPos = moveBy c pos -- narrowed by awaitMove
        case getAt newPos of
          'G' -> do
            void $ lift $ sendTextM "you win!"
          '#' -> do
            void $ lift $ sendTextM "to the wall? try again"
            gameConv' cancel pos
          '.' -> do
            gameConv' cancel newPos
          c -> do
            let Just d = toDigit c  -- no other characters in gameboard
            callCC $ \kill -> do
              modify $ I.insert d (kill ())
            gameConv' cancel newPos

startConv :: Handler
startConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "ok, starting the game"
  runCoroutineT $ gameConv startingPoint

usageConv :: Handler
usageConv = do
  (i,_) <- awaitText
  void $ replyTextM i $ "/start the game"

dispatcher :: Dispatcher
dispatcher = commandD "start" startConv
          <> fallbackD usageConv
