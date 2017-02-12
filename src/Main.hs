{-
  Module: Main
-}
module Main where
import Control.Monad
import System.IO
import Cli
main :: IO ()
main = do
         wd <- return root
         forever $ do
           putStr "$ "
           hFlush stdout
           command <- getLine
           result <- (return $ parseCmd wd command)
           wd <- return $ fst result
           putStrLn $ snd result

