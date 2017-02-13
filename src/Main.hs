{-
  Module: Main
-}
-- | The main module of the program
module Main where
import Control.Monad
import System.IO
import Cli
-- | The entry poing of the program
main :: IO () -- ^ Empty IO
main = cmd [root]

-- | The endless loop which will keep a state for the working directory
cmd :: [File] -- ^ List of states with head, the next working directory
    -> IO()   -- ^ Returning IO() because cmd is gonna run in main

cmd xs = do putStr "$ "
            hFlush stdout
            command <- getLine
            result <- (return $ parseCmd (head xs) command)
            putStrLn $ snd result
            cmd ((fst result):xs)
            return ()
