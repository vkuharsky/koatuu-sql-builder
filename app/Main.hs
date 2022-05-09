module Main where

import Data.Aeson ( eitherDecodeStrict' )
import KOATUU ()
import SqlBuilder ( produceSql )
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString as B
import qualified Data.Text.Lazy.Encoding as LTE
import qualified Data.Text.Encoding as E
import Options.Applicative.Simple (Parser, simpleOptions, strOption, 
                                   short, long, help, execParser, 
                                   info, helper, fullDesc, (<**>), 
                                   progDesc, header, option, showDefault, 
                                   value, metavar, auto)

data AppOptions =
  AppOptions { inputJson :: String
             , outputSql :: String
             , batchSize :: Int
             }

appOptions :: Parser AppOptions
appOptions = AppOptions
    <$> strOption
        ( long "inputJson"
       <> short 'i'
       <> help "Path to input file which contains json representation of KOATUU dictionary" )
    <*> strOption
        ( long "outputSql"
       <> short 'o'
       <> help "Path to output file which will contain sql commands to build KOATUU dictionary" )
    <*> option auto
        ( long "batchSize"
       <> short 'n'
       <> help "Set maximum number of insert commands generated per one batch in resulting file" 
       <> showDefault
       <> value 1000
       <> metavar "INT" )

buildDictionary :: AppOptions -> IO ()
buildDictionary (AppOptions input output n) =
  do
    koatuuJson <- B.readFile input
    case (eitherDecodeStrict' koatuuJson) of
      Left err -> print err
      Right val -> B.writeFile output $ E.encodeUtf8 $ produceSql n val

main :: IO ()
main = buildDictionary =<< execParser opts
       where 
         opts = info (appOptions <**> helper)
           ( fullDesc
          <> progDesc "Translates raw json file into a set of valid T-SQL commands suitable for building KOATUU dictionary."
          <> header "KoatuuSqlBuilder - import utility for KOATUU. (c)2020-2022. All rights reserved." )