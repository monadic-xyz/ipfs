module Main (main) where

import           System.FilePath.Glob (glob)
import           Test.DocTest

-- FIXME: there must be some way to parse these from the cabal file
defaultExtensions :: [String]
defaultExtensions =
    [ "DeriveGeneric"
    , "LambdaCase"
    , "MultiWayIf"
    , "NamedFieldPuns"
    , "RecordWildCards"
    , "StrictData"
    , "TupleSections"
    ]

main :: IO ()
main = do
    srcs <- glob "src/**/*.hs"
    doctest $ map ("-X" <>) defaultExtensions <> srcs
