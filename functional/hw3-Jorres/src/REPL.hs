module REPL where

import Control.Exception
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Typeable (Typeable)
import System.Directory
import System.FilePath
import System.IO

-- | Every operation is completed and exceptions handled within a context of
--   parameter `m` in this type alias, and 'StateT' handles preserving of the
--   current filepath between iterations of interactive REPL.
--   current filepath between iterations of interactive REPL.
--   current filepath between iterations of interactive REPL.
type MyFSStMonad m a = StateT FilePath m a

-- | General interface of what a monad needs to be successfully
--   implementing required tasks.
--   implementing required tasks.
--   implementing required tasks.
class (Monad m) => MyFSMonad m where
  mCheckAnythingExists :: FilePath -> m Bool
  mCheckDirectoryExists :: FilePath -> m Bool
  mCheckFileExists :: FilePath -> m Bool
  mListContents :: FilePath -> m [FilePath]
  mCreateDirectory :: FilePath -> m ()
  mTouch :: FilePath -> m ()
  mShortenPath :: FilePath -> m FilePath
  mGetPermissions :: FilePath -> m Permissions
  mReadFileContent :: FilePath -> m B.ByteString
  mRemoveDirectory :: FilePath -> m ()
  mRemoveFile :: FilePath -> m ()
  mFindFilesHere :: FilePath -> String -> m [FilePath]
  mWriteToFile :: FilePath -> String -> m ()
  mGetFileSize :: FilePath -> m Integer

  mFlush :: m ()
  mCatch :: m a -> (SomeException -> m a) -> m a
  mReadLine :: m String
  mWriteLine :: String -> m ()
  mWriteByteLine :: B.ByteString -> m ()
  mWriteLineNoWrap :: String -> m ()
  mWriteErrorMessage :: String -> m ()

-- | Implementation of operations, required for the task, for the IO monad.
--   That means that messages are printed to stdout, read from stdin, and
--   work with real filesystem is happening.
instance MyFSMonad IO where
  mCheckDirectoryExists = doesDirectoryExist
  mCheckAnythingExists = undefined
  mCheckFileExists = doesFileExist
  mListContents = listDirectory
  mCreateDirectory = createDirectory
  mGetPermissions = getPermissions
  mReadFileContent = B.readFile
  mRemoveDirectory = removeDirectory
  mRemoveFile = removeFile
  mFindFilesHere cur = findFiles [cur]
  mWriteToFile = writeFile
  mTouch arg = mWriteToFile arg ""
  mGetFileSize = getFileSize

  mFlush = hFlush stdout
  mCatch = catch
  mReadLine = getLine
  mWriteLine = putStrLn
  mWriteByteLine = B.putStrLn
  mWriteLineNoWrap = putStr
  mShortenPath = canonicalizePath
  mWriteErrorMessage = mWriteLine

-- | Helper function to print multiple lines. Uses 'mWriteLine' to print single line.
printLines :: (MyFSMonad m) => [String] -> m ()
printLines [] = return ()
printLines (x : xs) = do
  mWriteLine x
  printLines xs

-- | Prints contents of specified directory within a context of 'MyFSStMonad'.
processLS :: (MyFSMonad m) => [String] -> m ()
processLS arg = do
  lst <- mListContents $ head arg
  printLines lst

-- | The only processor that does not have 'generalCatch' wrapped around it and
--   working directly in 'MyFSStMonad', because it is the only processor that
--   changes state.
processCD :: (MyFSMonad m) => FilePath -> MyFSStMonad m ()
processCD toRaw = do
  to <- lift $ mShortenPath toRaw
  check <- lift $ mCheckDirectoryExists to
  if check
    then modify (const to)
    else lift $ mWriteErrorMessage "You've entered a nonexistent directory to cd. Please, try again"

-- | Wrapper for every processor operation except 'processCD'. Catches an
--   exception and prints it within a context of 'MyFSStMonad'.
generalCatch :: (MyFSMonad m) => m () -> m ()
generalCatch calc = mCatch calc (\e -> mWriteErrorMessage $ show (e :: SomeException))

-- | Prints file contents within a context of 'MyFSStMonad'
processCat :: (MyFSMonad m) => [String] -> m ()
processCat arg = do
  ln <- mReadFileContent $ head arg
  mWriteByteLine ln

-- | Creates an empty file, if a path before the final directory is
--   valid and all parent directories present.
processTouch :: (MyFSMonad m) => [String] -> m ()
processTouch arg = mTouch $ head arg

-- | Creates empty directory, if a path before the final directory is
--   valid and all parent directories present.
processMkdir :: (MyFSMonad m) => [String] -> m ()
processMkdir arg = mCreateDirectory $ head arg

-- | Removes directory recursively.
processRemoveDirectory :: (MyFSMonad m) => [String] -> m ()
processRemoveDirectory arg = mRemoveDirectory $ head arg

-- | Removes file.
processRemoveFile :: (MyFSMonad m) => [String] -> m ()
processRemoveFile arg = mRemoveFile $ head arg

-- | Helper function for 'processLocate', the step of the recursion.
recursiveSearch :: (MyFSMonad m) => String -> String -> m [String]
recursiveSearch cur what = do
  lst <- mListContents cur
  pairs <-
    forM
      lst
      ( \path -> do
          let full = cur </> path
          isDir <- mCheckDirectoryExists full
          if isDir
            then recursiveSearch full what
            else
              if path == what
                then return [full]
                else return []
      )

  return $ concat pairs

-- | Searches recursively for the supplied file within current directory,
--   printing full filepaths into the context of 'MyFSStMonad'.
processLocate :: (MyFSMonad m) => [String] -> m ()
processLocate (what : (cur : _)) = do
  lst <- recursiveSearch cur what
  printLines lst

-- | Prints supplied text to the supplied file. Overwrites existing content.
processEcho :: (MyFSMonad m) => [String] -> m ()
processEcho (arg : (cur : _)) = do
  mWriteLine "Enter the content to write to the file:"
  ln <- mReadLine
  mWriteToFile (cur </> arg) ln

-- | Helper function, turning a number of bytes into representation through bytes,
--   kbytes or mbytes.
formatCoolBytes :: Integer -> String
formatCoolBytes r
  | r < 2 ^ 10 = show r ++ " bytes"
  | r < 2 ^ 20 = show (div r (2 ^ 10)) ++ " kbytes"
  | r < 2 ^ 30 = show (div r (2 ^ 20)) ++ " mbytes"

-- | Calculates the size of the directory, recursively traversing its children.
goCalcSizeAmount :: (MyFSMonad m) => String -> m (Integer, Integer)
goCalcSizeAmount arg = do
  lst <- mListContents arg
  pairs <-
    forM
      lst
      ( \path -> do
          let full = arg </> path
          isDir <- mCheckDirectoryExists full
          if isDir
            then goCalcSizeAmount full
            else do
              sz <- mGetFileSize full
              return (sz, 1)
      )

  let res = foldl (\acc par -> (fst acc + fst par, snd acc + snd par)) (0, 0) pairs
  return res

-- | Prints info about the directory within the 'MyFSStMonad' context on invocation.
processDirinfo :: (MyFSMonad m) => [String] -> m ()
processDirinfo (arg : _) = do
  (sz, am) <- goCalcSizeAmount arg
  p <- mGetPermissions arg
  mWriteLineNoWrap "Path of the directory: "
  mWriteLine arg
  mWriteLineNoWrap "Permission rights: "
  mWriteLine $ show p
  mWriteLineNoWrap "Size of the folder + amount of files: "
  mWriteLine $ show (formatCoolBytes sz, am)

-- | Prints info about the file within the 'MyFSStMonad' context on invocation.
processFileinfo :: (MyFSMonad m) => [String] -> m ()
processFileinfo (arg : _) = do
  mWriteLineNoWrap "Path of the file: "
  mWriteLine arg
  p <- mGetPermissions arg
  mWriteLineNoWrap "Permission rights: "
  mWriteLine $ show p
  mWriteLineNoWrap "Size of the file: "
  sz <- mGetFileSize arg
  mWriteLine $ formatCoolBytes sz

-- | Prints help message within the 'MyFSStMonad' context on invocation.
processHelp :: (MyFSMonad m) => m ()
processHelp =
  printLines
    [ "help                            -- show this message again",
      "cp                              -- print current path you're on",
      "cd `dir`                        -- change current directory",
      "mkdir `dir`                     -- create new directory",
      "touch `filename`                -- create new empty file",
      "cat `filename`                  -- show file contents",
      "echo `filename` <ENTER> content -- change your directory",
      "dirinfo `dir`                   -- show directory information",
      "fileinfo `filename`             -- show file information",
      "rmfile `filename`               -- remove specified file",
      "rmdirectory `filename`          -- remove specified directory"
    ]

-- | General purpose wrapper around a processor of a command,
--   lifts to the context of the MyFSStMonad and
--   wraps into an exception catcher 'generalCatch'.
--   Used to supply one argument to the processor.
curGetTargetAndLiftWrapper :: (MyFSMonad m) => ([String] -> m ()) -> FilePath -> MyFSStMonad m ()
curGetTargetAndLiftWrapper calculation arg = do
  cur <- get
  target <- lift $ mShortenPath $ cur </> arg
  let args = [target]
  lift $ generalCatch $ calculation args

-- | General purpose wrapper around a processor of a command,
--   lifts to the context of the MyFSStMonad and
--   wraps into an exception catcher 'generalCatch'.
--   Used to supply two arguments to the processor.
curGetTargetAndLiftWrapperWithCur :: (MyFSMonad m) => ([String] -> m ()) -> FilePath -> MyFSStMonad m ()
curGetTargetAndLiftWrapperWithCur calculation arg = do
  cur <- get
  let locargs = [arg, cur]
  lift $ generalCatch $ calculation locargs

-- | A map that stores functions that process commands from the recognizable list.
--   Commands being:
--
--      * ls - 'processLS'
--      * help - 'processHelp'
--      * mkdir - 'processMkdir'
--      * touch - 'processTouch'
--      * cat - 'processCat'
--      * echo - 'processEcho'
--      * rmdirectory - 'processRemoveDirectory'
--      * rmfile - 'processRemoveFile'
--      * locate - 'processLocate'
--      * fileinfo - 'processFileinfo'
--      * dirinfo - 'processDirinfo'
listProcessors :: (MyFSMonad m) => M.Map String (FilePath -> MyFSStMonad m ())
listProcessors =
  M.fromList
    [ ("ls", curGetTargetAndLiftWrapper processLS),
      ("help", curGetTargetAndLiftWrapper $ const processHelp),
      ("mkdir", curGetTargetAndLiftWrapper processMkdir),
      ("touch", curGetTargetAndLiftWrapper processTouch),
      ("cat", curGetTargetAndLiftWrapper processCat),
      ("echo", curGetTargetAndLiftWrapperWithCur processEcho),
      ("dirinfo", curGetTargetAndLiftWrapper processDirinfo),
      ("fileinfo", curGetTargetAndLiftWrapper processFileinfo),
      ("locate", curGetTargetAndLiftWrapperWithCur processLocate),
      ("rmdirectory", curGetTargetAndLiftWrapper processRemoveDirectory),
      ("rmfile", curGetTargetAndLiftWrapper processRemoveFile)
    ]

-- | Entrypoint to the application when built with `stack build`.
--
--   Uses current user directory as a starting position in the tree.
singleEntry :: IO ()
singleEntry = do
  curDir <- getCurrentDirectory
  putStrLn curDir
  putStrLn "Consider typing `help` first."
  runStateT loop curDir
  return ()

-- | The main REPL function, user input processing happens there
loop :: (MyFSMonad m) => MyFSStMonad m ()
loop = do
  lift $ mWriteLineNoWrap ">> "
  lift mFlush
  cur <- get
  raw <- lift mReadLine
  unless
    (raw == "quit")
    ( do
        let command = takeWhile (not . isSpace) raw
        let rem = drop 1 $ dropWhile (not . isSpace) raw
        let id = elemIndex ' ' rem
        case id of
          (Just x) ->
            lift $ mWriteErrorMessage "You've got an invalid space somewhere in the input"
          Nothing ->
            if command == "cd"
              then processCD $ cur </> rem
              else
                if command == "curpath"
                  then lift $ mWriteLine cur
                  else
                    ( do
                        let valid = M.lookup command listProcessors
                        case valid of
                          Just method -> method rem
                          Nothing -> lift $ mWriteErrorMessage "No such command exists. Type `help` if you need help."
                    )
        loop
    )
