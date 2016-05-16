{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
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
          , ("results/:jobId", method POST $ addResult conn)
          , ("failed/:jobId", method GET $ failed conn)
          , ("refresh", method GET $ refresh conn)
          ]



isLeft4Dead jobId = runRedis conn $ 

refresh :: Connection -> Snap ()
refresh conn = do
    response <- liftIO $ runRedis conn $ lrange "inprogress" 0 (-1)
    case response of (Right inprogress) -> do
                         -- putback $ all inprogress that are missing "holds:<jobId>"                         
                         dead <- filterM () inprogress 
                         mapM_ (flip writeBS "\n") dead
                     (Left message) -> do
                         modifyResponse $ setResponseStatus 500 "Internal Server Error"


moveBack jobId = multiExec $ do
    rpoplpush "inprogress" "notcompleted"
    del [ BSC8.append "holds:" jobId ]


failed :: Connection -> Snap ()
failed conn = do
    Just jobId <- getParam "jobId"
    response <- liftIO $ runRedis conn $ moveBack jobId
    return ()


addResult :: Connection -> Snap ()
addResult conn = do
    Just output <- getPostParam "output"
    Just jobId <- getParam "jobId"
    -- TODO how to handle late results
    response <- liftIO $ runRedis conn $ multiExec $ do
        hmset (BSC8.append "job:" jobId) [ ("output", output)
                                         ]
        del $ [ BSC8.append "holds:" jobId ]
        lrem "inprogress" 0 jobId
        lpush "completed" [jobId]

    modifyResponse $ setResponseCode 201


getJob :: Connection -> Snap ()
getJob conn = do
    Just jobId <- getParam "jobId"
    response <- liftIO $ runRedis conn $ hget (BSC8.append "job:" jobId) "input"
    case response of (Right (Just input)) -> writeBS $ BSC8.append input "\n"
                     (Right Nothing) -> do
                         modifyResponse $ setResponseStatus 404 "Not Found"
                     (Left message) -> do
                         modifyResponse $ setResponseStatus 500 "Internal Server Error"


requestJob :: Connection -> Snap ()
requestJob conn = do
    response <- liftIO $ runRedis conn $ do
        rpoplpush "notcompleted" "inprogress"

    case response of (Right (Just jobId)) -> do
                         liftIO $ runRedis conn $ multiExec $ do
                             set (BSC8.append "holds:" jobId) "1"
                             expire (BSC8.append "holds:" jobId) 15
                         writeBS $ BSC8.append jobId "\n"
                     (Right Nothing) -> do
                         modifyResponse $ setResponseStatus 404 "Not Found"
                     (Left message) -> do
                         modifyResponse $ setResponseStatus 500 "Internal Server Error"


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
