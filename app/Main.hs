module Main (main) where

import           Data.Text.Lazy.IO as LT
import           Pass

main :: IO ()
main = do
    str <- LT.getContents
    doAllPass str
