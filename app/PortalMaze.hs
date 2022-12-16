{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
  deriving (Functor,Applicative,Monad,MonadCont,MonadIO)

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
        , T.empty
        , T.pack $ currentView!!0
        , "A " <> (T.pack $ currentView!!1) <> " D"
        , T.pack $ currentView!!2
        , T.empty
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

awaitMove :: (Int,Int) -> TgConv (Either Int Char)
awaitMove pos = do
  (id,m) <- awaitText
  if T.length m /= 1 then do
    void $ replyTextM id "enter one of WASD or a digit"
    awaitMove pos
  else do
    let c = T.head m
    case toDigit c of
      Just d -> do
        return $ Left d
      Nothing -> case moveBy c pos of
        Nothing -> do
          void $ replyTextM id "enter one of WASD or a digit"
          awaitMove pos
        Just _ ->
          return $ Right c

gameConv :: (Int,Int) -> MazeConv r ()
gameConv pos = do
  sendPanelView pos
  move <- lift $ awaitMove pos
  case move of
    Left d -> do
      maybekill <- gets (I.lookup d)
      case maybekill of
        Nothing -> do
          void $ lift $ sendTextM "this checkpoint is not unlocked"
          gameConv pos
        Just kill -> do
          kill
    Right c -> do
      let Just newPos = moveBy c pos
      case getAt newPos of
        'G' -> do
          void $ lift $ sendTextM "you win!"
        '#' -> do
          void $ lift $ sendTextM "to the wall? try again"
          gameConv pos
        '.' -> do
          gameConv newPos
        c -> do
          case toDigit c of
            Just d -> callCC $ \kill -> do
              modify $ I.insert d (kill ())
          gameConv newPos

startConv :: Handler
startConv = do
  (id,_) <- awaitText
  void $ replyTextM id $ "ok, starting the game"
  runCoroutineT $ gameConv startingPoint

usageConv :: Handler
usageConv = do
  (id,_) <- awaitText
  void $ replyTextM id $ "/start the game"

dispatcher :: Dispatcher
dispatcher = commandD "start" startConv
          <> fallbackD usageConv
