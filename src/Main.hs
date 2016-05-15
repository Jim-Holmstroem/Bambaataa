{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Hashable
import qualified Data.ByteString.Char8 as BSC8
import qualified Data.ByteString.Lazy.Char8 as BSLC8
import Control.Monad.IO.Class

import Database.Redis
import Snap


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
    response <- liftIO $ runRedis conn $ hget (BSC8.append "job:" jobId) "input"
    case response of (Right (Just input)) -> writeBS $ BSC8.append input "\n"
                     (Right Nothing) -> do
                         modifyResponse $ setResponseStatus 404 "Not Found"
                         writeBS "404 Not Found\n"
                     (Left message) -> do
                         modifyResponse $ setResponseStatus 500 "Internal Server Error"
                         writeBS "500 Error\n"


requestJob :: Connection -> Snap ()
requestJob conn = do
    response <- liftIO $ runRedis conn $ rpoplpush "notcompleted" "inprogress"
    case response of (Right (Just jobId)) -> writeBS $ BSC8.append jobId "\n"
                     (Right Nothing) -> do
                         modifyResponse $ setResponseStatus 404 "Not Found"
                         writeBS "404 Not Found\n"
                     (Left message) -> do
                         modifyResponse $ setResponseStatus 500 "Internal Server Error"
                         writeBS "500 Error\n"


addJob :: Connection -> Snap ()
addJob conn = do
    Just input <- getPostParam "input"
    let jobId = BSC8.pack $ show $ hash input

    response <- liftIO $ runRedis conn $ multiExec $ do -- TODO handling if this fails
        hmset (BSC8.append "job:" jobId) [ ("input", input)
                                         , ("n_retries", "0")
                                         ]
        lpush "notcompleted" [jobId] -- TODO handling duplicate hash

    modifyResponse $ setResponseCode 201
