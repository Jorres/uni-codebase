{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module REPLSpec where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M
import REPL
import System.Directory
import System.FilePath.Posix
import Test.Hspec

-- | Custom exception type, describing every expected incorrect situation
--   that can occur while running tests.
data MyEx
  = LsOnMissingDir
  | PermissionsOnNonExistentEntity
  | FileSizeOnNonExistentEntity
  | AttemptToReadNonExistentFile
  | AttemptToWriteToNonexistentFile
  | DeleteNonExistentDir
  | DeleteNonExistentFile
  | CreateDirOnNonExistentPath
  | CreateFileOnNonExistentPath
  | DirectoryExists
  | FileExists
  deriving (Show, Eq)

instance Exception MyEx

-- | Wrapper around directory entity, encapsulating map entity that stores
--   children of this directory.
newtype DirData x = DirData
  { _lst :: M.Map String x
  }
  deriving (Show, Eq)

makeLenses ''DirData

-- | Wrapper around file entity, encapsulating file properties
--   - name and content.
data FileParams = FileParams
  { _filename :: String,
    _content :: B.ByteString
  }
  deriving (Show, Eq)

makeLenses ''FileParams

-- | Node entity for the toy filesystem, can be either a directory or a file.
data MyFS = Dir (DirData MyFS) | File FileParams deriving (Show, Eq)

makePrisms ''MyFS

-- | Toy filesystem entity, contains directory structure and three buffers to check connection to
--   the input\output\exceptions data streams.
data MyFSPlus = MyFSPlus {_tree :: MyFS, _exc :: [String], _logs :: [String], _input :: [String]}

makeLenses ''MyFSPlus

-- | Type alias for the pure calculation that is running in tests. State stores filesystem state,
--   and ExceptT wraps a potential custom exception around this calculation.
type TestFSMonad = ExceptT MyEx (State MyFSPlus)

-- | Helper function, splits absolute filepath using 'System.Directory.splitDirectories' and
--   chops the root `/` element.
splitChopHome :: FilePath -> [FilePath]
splitChopHome fp = tail $ splitDirectories fp

-- | Helper function, generates Lens to the given filesystem node given absolute filepath.
generateWalkLens :: [String] -> Traversal' MyFSPlus MyFS -> Traversal' MyFSPlus MyFS
generateWalkLens [] res = res
generateWalkLens (curdir : dirs) prev = generateWalkLens dirs (prev . _Dir . lst . ix curdir)

-- | Helper function, wrapper around path preprocessing + Lens generation
accessor :: String -> Traversal' MyFSPlus MyFS -> Traversal' MyFSPlus MyFS
accessor fp = generateWalkLens $ splitChopHome fp

-- | Helper function, generates Lens to the given filesystem node and retrieves its value,
--   given absolute filepath.
tryWalkTree :: String -> TestFSMonad (Maybe MyFS)
tryWalkTree fp = do
  myfs <- get
  return $ myfs ^? accessor fp tree

-- | Helper function, checks following conditions and throws corresponding exceptions, if:
--
--  * parent directory for directory creation does not exist - 'CreateDirOnNonExistentPath'
--  * directory already exists - 'DirectoryExists'
--  * parent directory for file creation does not exist - 'CreateFileOnNonExistentPath'
--  * file already exists - 'FileExists'
checkCreationPreconditions :: FilePath -> MyEx -> MyEx -> TestFSMonad ([FilePath], FilePath)
checkCreationPreconditions fp ex1 ex2 = do
  let shinked = splitChopHome fp
  let (prepath, target : _) = splitAt (length shinked - 1) shinked
  isPrepath <- mCheckDirectoryExists $ '/' : joinPath prepath
  isFullPath <- mCheckAnythingExists fp
  if not isPrepath
    then throwError ex1
    else
      if isFullPath
        then throwError ex2
        else return (prepath, target)

-- | Helper function, throws 'DeleteNonExistentDir' or 'DeleteNonExistentFile' if
--   attempted to delete filesystem entity that does not exist.
checkDeletionPreconditions :: FilePath -> MyEx -> TestFSMonad ([FilePath], FilePath)
checkDeletionPreconditions fp ex1 = do
  let shinked = splitChopHome fp
  let (prepath, target : _) = splitAt (length shinked - 1) shinked
  isFullPath <- mCheckAnythingExists fp
  if not isFullPath
    then throwError ex1
    else return (prepath, target)

-- | Helper function that removes `..` from supplied path.
hRemoveDD :: [FilePath] -> [FilePath] -> FilePath
hRemoveDD [] acc = joinPath $ reverse acc
hRemoveDD (".." : rem) (prev : res) = hRemoveDD rem res
hRemoveDD (".." : rem) [] = hRemoveDD rem []
hRemoveDD (x : rem) acc = hRemoveDD rem (x : acc)

-- | Helper accessor that gives Traversal on 'M.Map' element (pointer to the next node in
--   filesystem tree).
mapElemAccessor :: Traversal' MyFSPlus MyFS -> String -> Traversal' MyFSPlus (Maybe MyFS)
mapElemAccessor t s = t . _Dir . lst . at s

-- | Implementation of operations, required for the task, for the custom filesystem
--  monad. That means that messages are appended to logs list, read from fixed input list,
--  and work with toy filesystem is happening, not influencing real filesystem in any way.
instance MyFSMonad TestFSMonad where
  mCheckAnythingExists fp = do
    node <- tryWalkTree fp
    return
      ( case node of
          (Just _) -> True
          _ -> False
      )

  mCheckDirectoryExists fp = do
    node <- tryWalkTree fp
    return
      ( case node of
          (Just (Dir _)) -> True
          _ -> False
      )

  mCheckFileExists fp = do
    node <- tryWalkTree fp
    return
      ( case node of
          (Just (File _)) -> True
          _ -> False
      )

  mListContents fp = do
    node <- tryWalkTree fp
    if node == Nothing
      then throwError LsOnMissingDir
      else
        ( do
            let (Just d) = node
            let maybeList = d ^? (_Dir . lst)
            return
              ( case maybeList of
                  Nothing -> []
                  (Just mp) -> M.keys mp
              )
        )

  mCreateDirectory fp = do
    (prepath, target) <- checkCreationPreconditions fp CreateDirOnNonExistentPath DirectoryExists
    let newdir = Dir (DirData {_lst = M.fromList []})
    mapElemAccessor (generateWalkLens prepath tree) target .= Just newdir

  mTouch fp = do
    (prepath, target) <- checkCreationPreconditions fp CreateFileOnNonExistentPath FileExists
    let newfile = File (FileParams {_filename = target, _content = B.pack ""})
    mapElemAccessor (generateWalkLens prepath tree) target .= Just newfile

  mRemoveDirectory fp = do
    (prepath, target) <- checkDeletionPreconditions fp DeleteNonExistentDir
    mapElemAccessor (generateWalkLens prepath tree) target .= Nothing

  mRemoveFile fp = do
    (prepath, target) <- checkDeletionPreconditions fp DeleteNonExistentFile
    mapElemAccessor (generateWalkLens prepath tree) target .= Nothing

  mGetPermissions fp = do
    anythingExists <- mCheckAnythingExists fp
    if anythingExists
      then return emptyPermissions
      else throwError PermissionsOnNonExistentEntity

  mGetFileSize fp = do
    anythingExists <- mCheckAnythingExists fp
    if anythingExists
      then return 100
      else throwError FileSizeOnNonExistentEntity

  mShortenPath fp = do
    let withDD = normalise fp
    let splitted = splitChopHome withDD
    return $ '/' : hRemoveDD splitted []

  mReadFileContent fp = do
    file <- mCheckFileExists fp
    myfs <- get
    if file
      then return $ myfs ^. (accessor fp tree . _File . content)
      else throwError AttemptToReadNonExistentFile

  mWriteToFile fp input = do
    file <- mCheckFileExists fp
    if file
      then accessor fp tree . _File . content .= B.pack input
      else throwError AttemptToWriteToNonexistentFile

  mFindFilesHere fp target = do
    lst <- mListContents fp
    return $ filter (== target) lst

  mFlush = return ()
  mCatch action handler = catchError action (handler . toException)
  mReadLine = do
    myfs <- get
    let curData = myfs ^. input
    case curData of
      (message : remainder) -> do
        input .= remainder
        return message
      _ -> return "quit"
  mWriteLine msg = do
    myfs <- get -- TODO investigate %=
    let logsD = myfs ^. logs
    -- logsD <- use logs          and this investigate, instead of 2 top lines
    logs .= (msg : logsD)
  mWriteByteLine msg = do
    myfs <- get -- TODO investigate %=
    let logsD = myfs ^. logs
    -- logsD <- use logs          and this investigate, instead of 2 top lines
    logs .= (B.unpack msg : logsD)

  mWriteLineNoWrap = mWriteLine
  mWriteErrorMessage msg = do
    myfs <- get
    let excD = myfs ^. exc
    exc .= (msg : excD)

-- | Test data, simple file.
simpleFile :: MyFS
simpleFile = File FileParams {_filename = "simpleName", _content = B.pack "sampleText"}

-- | Test data, simple directory containing one file.
simpleDir :: MyFS
simpleDir = Dir DirData {_lst = M.fromList [("file", simpleFile)]}

-- | Test data, bottomless recursive directory structure.
abcData :: MyFS
abcData =
  Dir
    DirData
      { _lst =
          M.fromList
            [ ("a", abcData),
              ("b", simpleDir),
              ("c", simpleFile)
            ]
      }

-- | Test data, simple folder with 3 'simpleDir' in it.
dirWithDirs :: MyFS
dirWithDirs =
  Dir
    DirData
      { _lst =
          M.fromList
            [ ("d1", simpleDir),
              ("d2", simpleDir),
              ("d3", simpleDir)
            ]
      }

-- | Test data, folder with 3 'dirWithDirs' in it.
nonRecursive2Lvl :: MyFS
nonRecursive2Lvl =
  Dir
    DirData
      { _lst =
          M.fromList
            [ ("d11", dirWithDirs),
              ("d21", dirWithDirs),
              ("d31", dirWithDirs)
            ]
      }

-- | Test data, wrapper around bottomless recursive directory structure.
testFiniteTree :: [String] -> MyFSPlus
testFiniteTree i = (MyFSPlus {_tree = nonRecursive2Lvl, _exc = [], _logs = [], _input = i})

-- | Test data, wrapper around bottomless recursive directory structure.
testRecursiveTree :: [String] -> MyFSPlus
testRecursiveTree i = (MyFSPlus {_tree = abcData, _exc = [], _logs = [], _input = i})

-- | Wrapper for REPL execution, running the calculation and then verifying
--   output with supplied checkers.
runCheckers ::
  FilePath ->
  MyFSPlus ->
  [MyFSPlus -> Expectation] ->
  (Either MyEx FilePath -> Expectation) ->
  IO ()
runCheckers stPath startTree checkers resChecker = do
  let x = execStateT loop stPath :: TestFSMonad FilePath
  let y = runExceptT x
  let (finalR, finalS) = runState y startTree
  forM_ checkers (\c -> c finalS)
  resChecker finalR

-- | Text message printed if unknown command printed.
noSuchCommand :: String
noSuchCommand = "No such command exists. Type `help` if you need help."

-- | Text message printed if 'processCD' invoked with invalid filesystem arguments.
noSuchDirectory :: String
noSuchDirectory = "You've entered a nonexistent directory to cd. Please, try again"

-- | Shortcut for prompt string.
pr :: String
pr = ">> "

-- | Test data, expected result of dirinfo command
simpleDirinfoList :: String -> [String]
simpleDirinfoList path =
  [ "Path of the directory: ",
    path,
    "Permission rights: ",
    "Permissions {readable = False, writable = False, executable = False, searchable = False}",
    "Size of the folder + amount of files: ",
    "(\"100 bytes\",1)"
  ]

-- | Test data, expected result of fileinfo command
simpleFileList :: String -> [String]
simpleFileList path =
  [ "Path of the file: ",
    path,
    "Permission rights: ",
    "Permissions {readable = False, writable = False, executable = False, searchable = False}",
    "Size of the file: ",
    "100 bytes"
  ]

-- | Test data, expected result of locate command on nonRecursive2Lvl
simpleLocateList :: [String]
simpleLocateList =
  [ "/d11/d1/file",
    "/d11/d2/file",
    "/d11/d3/file"
  ]

-- | Entry point for REPL tests
mainLibSpec :: IO ()
mainLibSpec = hspec $ do
  describe "Simple functionality:" $ do
    it "Check input prompt >>" $
      runCheckers
        "/"
        (testRecursiveTree ["quit"])
        [\s -> shouldBe (s ^. logs) [pr]]
        (\r -> shouldBe r (Right "/"))
    it "Check simple cd" $
      runCheckers
        "/"
        (testRecursiveTree ["cd a", "quit"])
        [\s -> shouldBe (s ^. logs) [pr, pr]]
        (\r -> shouldBe r (Right "/a"))
    it "Check ls" $
      runCheckers
        "/"
        (testRecursiveTree ["cd a", "ls .", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, pr, "a", "b", "c", pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/a"))
    it "Check mkdir" $
      runCheckers
        "/"
        (testRecursiveTree ["mkdir new", "cd new", "curpath", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, pr, pr, "/new", pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/new"))
    it "Check touch" $
      runCheckers
        "/"
        (testRecursiveTree ["touch newfile", "cat newfile", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, pr, "", pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/"))
    it "Check cat" $
      runCheckers
        "/"
        (testRecursiveTree ["cat a/a/b/file", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, "sampleText", pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/"))
    it "Check dirinfo" $
      runCheckers
        "/"
        (testRecursiveTree ["cd a", "dirinfo b", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) ([pr, pr] ++ simpleDirinfoList "/a/b" ++ [pr]),
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/a"))
    it "Check fileinfo" $
      runCheckers
        "/"
        (testRecursiveTree ["cd a", "fileinfo b/file", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) ([pr, pr] ++ simpleFileList "/a/b/file" ++ [pr]),
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/a"))
    it "Check locate" $
      runCheckers
        "/"
        (testFiniteTree ["cd d11", "locate file", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) ([pr, pr] ++ simpleLocateList ++ [pr]),
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/d11"))
    it "Check curpath" $
      runCheckers
        "/"
        (testRecursiveTree ["cd a/a/a/a/a/b/", "curpath", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) ([pr, pr] ++ ["/a/a/a/a/a/b"] ++ [pr]),
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/a/a/a/a/a/b"))
  describe "Complex situations:" $ do
    -- it "Check multi-level traversal" $
    --     runCheckers "/a/a"
    --         (testRecursiveTree ["cd /a/a/../../a/b", "curpath", "quit"])
    --         [\s -> shouldBe (reverse $ s ^. logs) [pr, pr, "/a/a/a/b", pr],
    --          \s -> shouldBe (s ^. exc) []]
    --         (\r -> shouldBe r (Right "/a/a/a/b"))
    it "Check create deep-nested dir" $ do
      let path = "a/a/a/a/a/a/a/target"
      runCheckers
        "/"
        (testRecursiveTree ["mkdir " ++ path, "cd " ++ path, "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, pr, pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right $ "/" ++ path))
    it "Check cd above launch directory" $
      runCheckers
        "/a/a"
        (testRecursiveTree ["cd ../../", "ls .", "quit"])
        [ \s -> shouldBe (reverse $ s ^. logs) [pr, pr, "a", "b", "c", pr],
          \s -> shouldBe (s ^. exc) []
        ]
        (\r -> shouldBe r (Right "/"))
  describe "Exception situations:" $ do
    it "Check error on nonexistent command" $
      runCheckers
        "/"
        (testRecursiveTree ["gibberish", "quit"])
        [ \s -> shouldBe (s ^. logs) [pr, pr],
          \s -> shouldBe (s ^. exc) [noSuchCommand]
        ]
        (\r -> shouldBe r (Right "/"))
    it "Check error cd on nonexistent directory" $
      runCheckers
        "/"
        (testRecursiveTree ["cd i-do-not-exist", "quit"])
        [ \s -> shouldBe (s ^. logs) [pr, pr],
          \s -> shouldBe (s ^. exc) [noSuchDirectory]
        ]
        (\r -> shouldBe r (Right "/"))
    it "Cannot create dir without parent structure" $
      runCheckers
        "/"
        (testRecursiveTree ["mkdir nonexistent/target", "quit"])
        [ \s -> shouldBe (s ^. logs) [pr, pr],
          \s -> shouldBe (s ^. exc) ["CreateDirOnNonExistentPath"]
        ]
        (\r -> shouldBe r (Right "/"))
    it "Cannot create file without parent structure" $
      runCheckers
        "/"
        (testRecursiveTree ["touch nonexistent/file.txt", "quit"])
        [ \s -> shouldBe (s ^. logs) [pr, pr],
          \s -> shouldBe (s ^. exc) ["CreateFileOnNonExistentPath"]
        ]
        (\r -> shouldBe r (Right "/"))

-- TODO %=, %~, .=, .~ investigate
