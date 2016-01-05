module Revision_19 where

import Data.List (sortBy)
import qualified Data.ByteString.Char8 as BC

main = BC.getLine >> BC.getContents
    >>= mapM_ BC.putStrLn . sortBy comp . BC.lines

comp :: BC.ByteString -> BC.ByteString -> Ordering
comp a b =
    case compare (BC.maximum a) (BC.maximum b) of
        LT -> LT
        GT -> GT
        _  -> compare a b

