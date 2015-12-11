module HR_Lab10 where

import Data.List (sortBy)
import qualified Data.ByteString.Char8 as BC

main = BC.getLine >> BC.getContents >>= mapM_ BC.putStrLn . sortBy comparison . BC.lines

comparison :: BC.ByteString -> BC.ByteString -> Ordering
comparison a b
    | BC.length a < BC.length b = LT
    | BC.length a > BC.length b = GT
    | otherwise                 = compare a b

