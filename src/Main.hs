import App
import Web.Scotty
import System.Environment
import Control.Applicative
import qualified DataTypes as DT
import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y

main :: IO ()
main = do
        dbinfoMaybe <- loadConfigFile
        case dbinfoMaybe of
            Nothing -> error ("Could not parse config file. Please define a config.yml file in the location " ++
                              "defined in the BRAINCANDY_CONFIG_DIR environment variable.\n" ++
                              "A sample config file can be found under the data directory.")
            (Just dbinfo) -> scotty 3000 (endpoints dbinfo)

getConfigPath :: IO String
getConfigPath = do
    return (++) <*> (getEnv "BRAINCANDY_CONFIG_DIR") <*> return ("/" ++ "config.yml")

loadConfigFile :: IO (Maybe DT.DatabaseConnectionInfo)
loadConfigFile = do
    configFile <- getConfigPath
    yamlText <- BS.readFile  configFile
    return (Y.decode yamlText :: Maybe DT.DatabaseConnectionInfo)
