{-# LANGUAGE DeriveFunctor #-}

module Main (main) where

import Data.ByteString
import Network.Wreq
import Data.String.Conversions
import Control.Lens
import qualified Lib2
import qualified Lib3
import Data.IORef

data MyDomainAlgebra next
  = Load (() -> next)
  | AddArtwork String (() -> next)
  | Save (() -> next)
  deriving (Functor)

type MyDomain = Free MyDomainAlgebra

load :: MyDomain ()
load = liftF $ Load id

main :: IO ()
main = do
    let rawRequest = cs "works?" :: ByteString
    resp <- post "http://localhost:3000" rawRequest
    putStrLn $ cs $ resp ^. responseBody