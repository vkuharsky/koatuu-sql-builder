{-# LANGUAGE OverloadedStrings #-}
module KOATUU 
    ( KOATUU(..)
    ) where

import Control.Applicative
import Control.Monad(when)
import Data.Aeson ( FromJSON(parseJSON), (.:), withObject )
import Data.Aeson.Types
    ( FromJSON(parseJSON), (.:), withObject, Parser )
import Data.Foldable(asum)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.Int as B
import Text.Read(readMaybe)
import qualified Data.Aeson.Encoding as B
import Data.Text.Internal.Read (IReader)
import qualified Data.Text.Lazy.Builder.Int as Reader
import Data.Text.Read (decimal)

data KOATUU =
    KOATUU { code :: T.Text         -- object's id field according to KOATUU
           , category :: T.Text     -- object's category type
           , name :: T.Text         -- object's ukrainian name
           } deriving Show

-- translates raw Int values into lazy text Parser instances 
packInteger :: Int -> LT.Text  
packInteger =  B.toLazyText . B.decimal

-- number convertion builder
convertNum :: Int -> Parser LT.Text
convertNum = return . packInteger

-- 'replace apostrophe' builder function
repApo :: LT.Text -> Parser LT.Text
repApo = return . LT.replace "'" "''"

-- FromJSON typeclass instance for KOATUU data type
instance FromJSON KOATUU where
    parseJSON = withObject "KOATUU" $ \obj -> do
        codeL1 <- asum [
            obj .: "Перший рівень",
            obj .: "Перший рівень" >>= convertNum ] 
        codeL2  <- asum [
            obj .: "Другий рівень",
            obj .: "Другий рівень" >>= convertNum ]
        codeL3  <- asum [
            obj .: "Третій рівень",
            obj .: "Третій рівень" >>= convertNum ]
        codeL4  <- asum [
            obj .: "Четвертий рівень",
            obj .: "Четвертий рівень" >>= convertNum ]
        cat     <- obj .: "Категорія"
        ukrName <- obj .: "Назва об'єкта українською мовою" >>= repApo

        let empty =  LT.pack ""
            code  =  LT.toStrict $ head $ filter (/= empty) [ codeL4, codeL3,  codeL2,  codeL1]
            ukrName' = LT.toStrict ukrName

        return $ KOATUU  code cat  ukrName'