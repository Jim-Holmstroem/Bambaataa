{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Hashable
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Control.Monad.IO.Class

import Database.Redis
import Snap

-- TODO does it handle utf8?
-- FIXME Unable to add a jobb when the queue is empty and a worker is requesting a Job

main :: IO ()
main = do
    conn <- connect defaultConnectInfo
    quickHttpServe $ site conn


site :: Connection -> Snap ()
site conn =
    route [ ("jobs", method POST $ addJob conn)
          , ("jobs/:jobId", method GET $ getJob conn)
          , ("requestJob", method GET $ requestJob conn)
          ]


getJob :: Connection -> Snap ()
getJob conn = do
    Just jobId <- getParam "jobId"
    response <- liftIO $ runRedis conn $ hget "job:" jobId
    case response of (Right (Just input)) -> writeBS input


requestJob :: Connection -> Snap ()
requestJob conn = do
    response <- liftIO $ runRedis conn $ rpoplpush "notcompleted" "inprogress"
    case response of (Right (Just jobId)) -> writeBS jobId
                     (Right Nothing) -> notFound ""
                     (Left message) -> serverError message


addJob :: Connection -> Snap ()
addJob conn = do
    input <- readRequestBody (256 * 2^20)
    let jobId = BSC8.pack $ show $ hash input
    response <- liftIO $ runRedis conn $ multiExec $ do -- TODO handling if this fails
        hmset ("job:") [ ("input", "poop")
                       , ("n_retries", "0")
                       ]
        lpush "notcompleted" [jobId] -- TODO handling duplicate hash

    writeBS $ "ok"




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
