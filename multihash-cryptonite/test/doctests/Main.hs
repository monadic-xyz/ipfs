module Main (main) where

import           System.FilePath.Glob (glob)
import           Test.DocTest

main :: IO ()
main = do
    srcs <- glob "src/**/*.hs"
    doctest srcs
