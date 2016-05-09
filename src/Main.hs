{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class

import Database.Redis
import Snap

-- TODO utf8


main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    quickHttpServe $ site conn


site :: Connection -> Snap ()
site conn =
    route [ ("set/:key/:value", setHandler conn)
          , ("get/:key", getHandler conn)
          ]

setHandler :: Connection -> Snap ()
setHandler conn = do
    Just key <- getParam "key"
    Just value <- getParam "value"
    liftIO $ runRedis conn $ set key value
    writeBS key


getHandler :: Connection -> Snap ()
getHandler conn = do
    Just key <- getParam "key"
    Right (Just value) <- liftIO $ runRedis conn $ get key
    writeBS value
