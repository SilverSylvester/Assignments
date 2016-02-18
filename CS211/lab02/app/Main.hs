module Main where

import Control.Arrow ((&&&))
import Data.List (sortBy)
import Data.Ord (comparing)
import qualified Data.ByteString.Char8 as BC

-- Plaintext version of Wikipedia:
-- http://kopiwiki.dsd.sztaki.hu/

main = BC.getContents >>=
    mapM_ (putStr . (:[]) . snd) . sortBy (flip (comparing fst))
    . map (BC.length &&& BC.head) . BC.group . BC.sort >> putStr "\n"

