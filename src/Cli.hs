{-
  Module: Cli  
-}
module Cli(parseCmd, splitOn, root, File) where

import Utils
import File
import Data.Either
import Data.Maybe
-- | Parses the command, passes it for execution and returns the value
parseCmd :: File   -- ^ Current directory
         -> String -- ^ The comand and it's parameters
         -> (File, String) -- ^ The new current directory and the result string
parseCmd wd string = execCmd cmd
  where cmd     = (wd, head cmdLst, tail cmdLst)
        cmdLst  = splitOn string ' '

-- | Executes the command and return the result
execCmd :: (File, String, [String]) -- ^ (Working dir, command, arguments)
        -> (File, String) -- ^ working directory and Result string to be shown on output

-- ls
execCmd (wd, "ls", [])  = (wd, fileNames wd)
execCmd (wd, "ls", [path])  = either (buildResponse) (informError) (cd path wd)
  where informError msg     = (wd, msg)
        buildResponse newWd = (wd, fileNames newWd)
execCmd (wd, "ls", _) = (wd, "Too many arguments.")

-- cd
execCmd (wd, "cd", []) = (root, "") 
execCmd (wd, "cd", [path])  = either (buildResponse) (informError) (cd path wd)
  where informError msg     = (wd, msg)
        buildResponse newWd = (newWd, "")
execCmd (wd, "cd", _) = (wd, "Too many arguments")


-- cat
execCmd (wd, "cat", []) = (wd, "The no-arg version of cat is still in development")
execCmd (wd, "cat", argLst) = (wd, resultString)
  where resultString = foldr (++) "" $ map (++ ['\n']) resultsForEach
        resultsForEach = map (cat) $ lefts eitherFiles
        eitherFiles = map (flip getFileByPath wd) argLst


-- pwd
execCmd (wd, "pwd", []) = pwd wd
  where pwd cwd
          | cwd == root = (wd, "/")
          | otherwise   = (wd, listToPath $ pwd1 wd [])
        pwd1 :: File -> [File] -> [File]
        pwd1 cwd result 
          | cwd == root = result
          | otherwise   = pwd1 (fromJust $ getParent cwd) (cwd:result)


-- Keep this one last
execCmd (wd, invalidCmd, _) = (wd, "Invalid command " ++  invalidCmd)
