{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad.IO.Class(liftIO)
import Data.String.Conversions
import Web.Scotty

main :: IO ()
main = scotty 3000 $
    post "/" $ do
        b <- body
        liftIO $ putStrLn $ concat ["Request was: ", cs b]
        text $ "This is response"

type AppState = (TVar Lib2.State, Chan Lib3.StorageOp)
cmd :: String -> StateT AppState IO String
cmd str = do
  case Lib2.parse Lib3.parseCommand str of
    (Left e, _) -> liftIO $ return ("PARSE ERROR: " ++ e)
    (Right c, "") -> do
      (st, chan) <- S.get
      tr <- liftIO $ Lib3.stateTransition st c chan
      case tr of
        Left e2 -> return ("ERROR: " ++ e2)
        Right m -> do
          case m of
            Just msg -> return msg
            Nothing -> return ""
    (Right _, r) -> return ("PARSE ERROR: string is not fully consumed - " ++ r)
