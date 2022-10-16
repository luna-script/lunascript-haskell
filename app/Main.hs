module Main (main) where

import           Data.Text.Lazy.IO as LT
import           Path

main :: IO ()
main = do
    str <- LT.getContents
    doAllPath str
