{-# LANGUAGE DeriveGeneric #-}

module DataTypes (DatabaseConnectionInfo(..), JQuote(..), JError(..)) where

import GHC.Generics
import qualified Data.Text as T
import Data.Word
import Data.Aeson

data DatabaseConnectionInfo = ConnectionInfo  { hostname :: !T.Text, port :: Word16, username :: !T.Text, password :: !T.Text, database :: !T.Text } deriving (Show, Generic)

data JQuote  = JQuote { _id :: Int, quote :: !T.Text, attribution :: !T.Text } deriving (Show, Generic)

data JError = JError { error :: !T.Text } deriving (Show, Generic)

instance ToJSON JQuote
instance ToJSON JError
instance FromJSON DatabaseConnectionInfo