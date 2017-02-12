{-
  Module: Utils  
-}

module Utils(splitOn) where

splitOn :: String -- ^ The string which will be splitted
        -> Char   -- ^ The delimiter
        -> [String] -- ^ Result
splitOn str delim = splitOn1 str delim "" []
  where splitOn1 "" _ curr res = filter (not . null) $ res ++ [curr]
        splitOn1 (c:cs) delim curr res
          | c == delim = splitOn1 cs delim "" (res ++ [curr])
          | otherwise  = splitOn1 cs delim (curr ++ [c]) res
