{-# LANGUAGE OverloadedStrings #-}

module DB (findTodaysQuote, createNewQuoteEntry, hasDatabaseBeenSeeded) where

 import Database.MySQL.Simple
 import Data.Int (Int64)
 import Control.Monad.Reader
 import Safe
 import DataTypes

 hasDatabaseBeenSeeded :: ReaderT Connection IO Bool
 hasDatabaseBeenSeeded = do
    conn <- ask
    [Only xs] <- liftIO $ query_ conn "SELECT COUNT(1) FROM QUOTE"
    return $ if (xs :: Int) > 0 then True else False

 findTodaysQuote :: ReaderT Connection IO (Maybe JQuote)
 findTodaysQuote = do
     conn <- ask
     xs <- liftIO $ query_ conn "SELECT ID,QUOTE,ATTRIB FROM QUOTE WHERE ID IN (SELECT QID FROM ROUND WHERE QDATE = CURRENT_DATE())"
     return $ fmap (\(_id, quote, attrib) -> JQuote _id quote attrib) (headMay xs)

 createNewQuoteEntry :: ReaderT Connection IO JQuote
 createNewQuoteEntry = do
    rid <- findMaxRoundId
    findNextQuote rid
    where
        findNextQuote :: Int -> ReaderT Connection IO JQuote
        findNextQuote roundId = do
            maybeQ <- findUnusedQuoteForRound roundId
            case maybeQ of
                (Just quote) -> do
                    insertQuoteIntoRound quote roundId
                    return quote
                Nothing -> findNextQuote (roundId + 1)


 insertQuoteIntoRound ::  JQuote -> Int -> ReaderT Connection IO Int64
 insertQuoteIntoRound (JQuote _id _ _) roundId = do
    conn <- ask
    liftIO $ execute conn "INSERT INTO ROUND (RID,QID, QDATE) VALUES (?, ?, CURRENT_DATE())" (roundId, _id)

 findMaxRoundId :: ReaderT Connection IO Int
 findMaxRoundId = do
    conn <- ask
    [(Only maxId)] <- liftIO $ query_ conn "SELECT IFNULL(MAX(RID),1) FROM ROUND"
    return maxId

 findUnusedQuoteForRound :: Int -> ReaderT Connection IO (Maybe JQuote)
 findUnusedQuoteForRound round = do
    conn <- ask
    xs <- liftIO $ query conn "SELECT ID,QUOTE,ATTRIB FROM QUOTE WHERE ID NOT IN (SELECT QID FROM ROUND WHERE RID = ?) ORDER BY RAND() LIMIT 1" (Only round)
    (return . fmap (\(_id, quote, attrib) -> JQuote _id quote attrib) . headMay) xs







