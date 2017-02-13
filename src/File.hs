{- |
  Module: File
-}
module File(
            File(..),
            isRoot,
            fileNames,
            pathToList,
            listToPath,
            getFileByName,
            getFileByPath,
            getParent,
            file4,
            cd,
            cat,
            root
           )
where

import Data.List
import Data.Maybe
import Data.Either

-- | Representation of a file in a filesystem
data File
  -- | Normal file with text content inside
  = NormalFile {
                 name :: String, -- ^ The name of the file
                 content :: String -- ^ The file contents
               }
  -- | Directory containing other directories or Normal files. The root
  -- directory will have empty file name "", which might lead to some confusing
  -- error messages
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
-- | Transform path string to list of filenames
pathToList :: String   -- ^ String representing the path
           -> [String] -- ^ List of filenames
pathToList str
  | last str == '/' = pathToList1 (init str) "" []
  | otherwise = pathToList1 str "" []
  where pathToList1 str@(char:rest) currentFilename lstOfFilenames
          | null rest   = reverse ((currentFilename ++ [char]) : lstOfFilenames)
          | char == '/' = pathToList1 rest "" (currentFilename : lstOfFilenames)
          | otherwise   = pathToList1 rest (currentFilename ++ [char]) lstOfFilenames

cd :: String -- ^ Path to the directory
   -> File   -- ^ Current dir
   -> Either File String   -- ^ The resulting directory on the left or error message on the right
cd path wd = either (checkDir) (Right . id) (getFileByPath path wd)
  where checkDir (NormalFile _ _) = Right ("cd: not a directory:" ++ path)
        checkDir dir@(Directory _ _) = Left dir

-- | Get a file from a directory by it's name
getFileByName :: File -- ^ Dirtectory from which we want to get the file
              -> String -- ^ Filename
              -> Either File String -- ^ The wanted file or error message
getFileByName (NormalFile dName _) fName = Right $ dName ++ " isn't a directory"
getFileByName (Directory dirName files) fName
  | fName `elem` (map name files) = Left $ head $ filter byName files
  | otherwise                     = Right $ fName ++ " is not in " ++ dirName
  where byName = ((fName ==) . name)

-- | Get a file by its relative path to the current working directory
getFileByPath :: String              -- ^ Path to the file
              -> File                -- ^ Working directory
              -> Either File String  -- ^ The required file on the left or error message on the right
getFileByPath ""   _  = Right "No path given"
getFileByPath path wd 
  | (length path == 1) && (head path == '/') = Left root 
  | head path == '/' = gFile (pathToList $ tail path) root
  | otherwise = gFile (pathToList path) wd
  where gFile :: [String] -- ^ List of fileNames to traverse to reach the wanted
              -> File   -- ^ Working directory
              -> Either File String   -- ^ File, with the needed name from the working dir

        gFile []        wd = Right "File not found"
        gFile ["."]     wd = Left wd
        gFile (".":rs)  wd = gFile rs wd
        gFile [".."]    wd = Left $ fromJust $ getParent wd
        gFile ("..":rs) wd = gFile rs $ fromJust $ getParent wd
        gFile [fName]  wd = either (Left) (notFound) (getFileByName wd fName)
        gFile (fn:rs)  wd = either (gFile rs) (notFound) (getFileByName wd fn)
        notFound _ = Right ("Cannot find file: " ++ path)

-- | Get the contents of a Normal File or return error if the file isn't normal
cat :: File -- ^ File, which content shall be shown
    -> String -- ^ The content of the file or the error message that first arg. is not a NormalFile

cat f@(NormalFile _ _) = content f
cat f                  = name f ++ " is not a normal file."

-- | Return the parent of the file or Nothing if not found
-- This way of searching for the element may cause problem if the name and the
-- contents of the file / directory are the same
getParent :: File -- ^ The file that we search from the root
          -> Maybe File -- ^ The file's parent or Nothing if not found
getParent file
  | file == root = Just root
  | otherwise    = search file root
  where search file wd 
          | file `elem` (files wd) = Just wd
          | length dirs > 0 = if (length dirsParse > 0) 
                                then (Just $ head dirsParse)
                                else Nothing
          | otherwise = Nothing
          where dirs = filter isDirectory $ files wd
                dirsParse = catMaybes $ map (search file) dirs

-- | Check if the argument given is a NormalFile
isNormalFile :: File -- ^ The file to check
             -> Bool -- ^ Result of the check
isNormalFile (NormalFile _ _) = True
isNormalFile _                = False

-- | Check if the argument given is a Directory
isDirectory :: File -- ^ The file to check
            -> Bool -- ^ Result of the check
isDirectory (Directory _ _) = True
isDirectory _               = False

-- | Returns string representing a path from a list of files
listToPath :: [File] -- ^ The mentioned list
           -> String -- ^ The result

listToPath lst = helper lst ""
  where helper :: [File] -> String -> String
        helper [] result     = result
        helper (x:xs) result = helper xs (result ++ ['/']++(name x))

-- Example file system

file1 = NormalFile "file1" "File1's content"
file2 = NormalFile "file2" "File2's content"
file3 = NormalFile "file3" "File3's content"
file4 = NormalFile "file4" "File4's content"
dir1  = Directory "nkotsev" [file3, file4]
dir2  = Directory "home" [dir1, file2]


-- | Root file of the filesystem
root :: File
root  = Directory "" [dir2, file1]
