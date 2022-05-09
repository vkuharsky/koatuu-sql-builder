module SqlBuilder (produceSql) where

import qualified Data.Text as T

import Data.Monoid((<>))
import KOATUU
import Data.Foldable(foldl')

import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Internal.Builder as T


buildTable :: B.Builder
buildTable = B.fromString
                      "IF OBJECT_ID(N'#KoatuuInsert') IS NOT NULL \n\
                    \   DROP TABLE #KoatuuInsert; \n\
                    \ \nCREATE TABLE #KoatuuInsert \n\
                    \ (\n\
                    \     [Code] NVARCHAR(10) NOT NULL PRIMARY KEY, \n\
                    \     [Type] NCHAR(1) NOT NULL, \n\
                    \     [Name] NVARCHAR(1024) NOT NULL \n\
                    \ );\n"

insertStatement :: B.Builder
insertStatement = B.fromString "INSERT INTO #KoatuuInsert([Code], [Type], [Name]) VALUES "

valuesDelimiter :: B.Builder
valuesDelimiter = B.fromString "', '"

leadingStatement :: B.Builder
leadingStatement = B.fromString "\n('"

trailingStatement :: B.Builder
trailingStatement = B.fromString "')"

emptyBuilder :: B.Builder
emptyBuilder = B.fromString ""

endOfBatch :: B.Builder
endOfBatch = B.fromString ";\n"

statementDelimiter :: B.Builder
statementDelimiter = B.singleton ','

buildRecord :: KOATUU -> B.Builder
buildRecord (KOATUU code category name) =
    let code' = B.fromText code <> valuesDelimiter
        category' = B.fromText category <> valuesDelimiter
        name' = B.fromText name
     in leadingStatement <> code' <> category' <> name' <> trailingStatement

chunk' :: Int -> [a] -> [[a]]
chunk' _ [] = []

chunk' n xs | n > 0 = let xs' = take n xs
                          ys' = drop n xs
                       in xs' : (ys' `seq` xs' `seq` chunk' n ys') --(seq ys' $! chunk' n ys')
            | otherwise = error "Number of items in chunk must be positive integer"

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []

chunk n xs = take n xs : chunk n (drop n xs)

builderToText :: B.Builder -> T.Text
builderToText = LT.toStrict . B.toLazyText

buildBatch :: [KOATUU] -> B.Builder
buildBatch [] = B.fromString ""

buildBatch [x] =  buildRecord x <> endOfBatch

buildBatch (x : xs) =
    let record = buildRecord x
        acum   = seq record $! record <> statementDelimiter
     in acum <> buildBatch xs

produceSql :: Int -> [KOATUU]  -> T.Text
produceSql n xs =
    let batch = chunk n xs
        f a c = a <> insertStatement <> buildBatch c
        statements = foldl' f emptyBuilder batch
     in builderToText $ buildTable <> statements

