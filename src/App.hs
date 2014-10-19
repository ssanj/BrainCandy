{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module App (endpoints) where

import Web.Scotty as SC
import Data.Aeson
import GHC.Generics
import System.IO
import qualified Data.Text as T
import Control.Exception.Base
import Database.MySQL.Simple
import DB
import Control.Monad.Reader
import DataTypes

endpoints :: DatabaseConnectionInfo -> ScottyM ()
endpoints dbci = do
        homePage
        resources
        quoteEndpoint dbci

homePage :: ScottyM ()
homePage = get "/" $ file "./src/resources/index.html"

-- TODO: Validate path. Only allow relatives. No navigation ".."
resources :: ScottyM ()
resources = get "/resources/:cat/:resource" $ do
    c <- param "cat"
    r <- param "resource"
    file ("./src/resources/" ++ c ++ "/" ++ r)

quoteEndpoint :: DatabaseConnectionInfo -> ScottyM ()
quoteEndpoint dbci =  get "/quote" $ do
                    setHeader  "Content-Type" "application/json"
                    maybeQ <- liftIO (getQuote dbci)
                    case maybeQ of
                        (Just quote) -> raw $ encode quote
                        Nothing -> SC.json $ JError "Could not retrieve quote"

getQuote :: DatabaseConnectionInfo -> IO (Maybe JQuote)
getQuote dbci = catch (bracket (getConnection dbci) close (\conn -> do
            isSeeded <- runReaderT hasDatabaseBeenSeeded conn
            case isSeeded of
                True -> do
                    maybeQ <- runReaderT findTodaysQuote conn
                    case maybeQ of
                        v@(Just _) -> return v
                        Nothing -> fmap Just $ runReaderT createNewQuoteEntry conn
                False -> Prelude.error "Please seed the database."
            )) (\(SomeException e) -> do
                            putStrLn $ "got an error: " ++ (show e)
                            return Nothing)

getConnection :: DatabaseConnectionInfo -> IO Connection
getConnection (ConnectionInfo  hostname port username  password database)  =
    connect defaultConnectInfo { connectHost = T.unpack hostname, connectPort = port ,connectUser = T.unpack username, connectPassword = T.unpack password, connectDatabase = T.unpack database}
