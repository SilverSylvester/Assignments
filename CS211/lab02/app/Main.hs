module Main where

import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as BC

main = BC.getContents >>=
    putStrLn . map BC.head . sortBy (flip (comparing BC.length))
    . BC.group . BC.sort

