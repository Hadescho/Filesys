{- |
  Module: File
-}
module File(
            File(..),
            isRoot
           )
where

import Data.List

data File
  -- | Normal file with text content inside
  = NormalFile {
                 name :: String, -- ^ The name of the file
                 content :: String -- ^ The file contents
               }
  -- | Directory containing other directories or Normal files. The root 
  -- directory will have empty file name ""
  | Directory  {
                 name :: String, -- ^ The name of the directory
                 files :: [File] -- ^ The files contained in the directory
               } deriving (Eq, Show)

-- | Check if the File is the root directory
isRoot :: File  -- ^ The file that we want to check
       -> Bool  -- ^ Will return true if the current directory is root
isRoot (Directory "" _) = True
isRoot (Directory _  _) = False
isRoot (NormalFile _ _) = error "The file should be a Directory"

-- | Return a concatenation of the names of the files and directories of the
--   given directory
fileNames :: File -- ^ The directory which contents we want
          -> String -- ^ The resulting string
fileNames (Directory _ files) = intercalate "  " $ map name files  
fileNames (NormalFile _ _) = error "should be directory"




-- | Example file system

file1 = NormalFile "file1" "File1's content"
file2 = NormalFile "file2" "File2's content"
file3 = NormalFile "file3" "File3's content"
file4 = NormalFile "file4" "File4's content"
dir1  = Directory "nkotsev" [file3, file4]
dir2  = Directory "home" [dir1, file2]
root  = Directory "" [dir2, file1]
emptydir = Directory "emptydir" []
