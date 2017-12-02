--module CountEntries (listDirectory, countEntriesTrad) where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM, forM_, when)
import Control.Monad.Writer (WriterT, execWriterT, tell)
import Control.Monad.Trans (liftIO)
import Control.Monad.State
import Control.Monad.Reader
import Data.List

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents where
  notDots p = p /= "." && p /= ".."

--printDirectory :: IO [String] -> IO ()
printDirectory dirs = do
  d <- dirs
  putStrLn . show $ d

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
              then countEntriesTrad newName
              else return []
  return $ (path, length contents) : concat rest

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName

data AppConfig = AppConfig { cfgMaxDepth :: Int } deriving (Show)
data AppState = AppState { stDeepestReached :: Int } deriving (Show)
type App a = ReaderT AppConfig (StateT AppState IO) a

runApp :: App a -> Int -> IO (a, AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int, Int)]
constrainedCount curDepth path = do
  contents <- liftIO $ listDirectory $ path
  cfg <- ask
  maxDepth <- get
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put AppState { stDeepestReached = newDepth }
        constrainedCount newDepth newPath
      else return []
  return $ (path, length contents, stDeepestReached maxDepth) : concat rest

--data AppLog = AppLog { getLog :: [(FilePath, Int)] } deriving (Show)
type AppLog = [(FilePath, Int)] 
type AppW a = ReaderT AppConfig (StateT AppState (WriterT AppLog IO)) a
runAppW :: AppW a -> Int -> IO AppLog
runAppW k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in execWriterT (runStateT (runReaderT k config) state)

constrainedCountW :: Int -> FilePath -> AppW ()
constrainedCountW curDepth path = do
  contents <- liftIO $ listDirectory $ path
  cfg <- ask
  tell $ [(path, length contents)]
  forM_ contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    when (isDir && curDepth < cfgMaxDepth cfg) $ do
      let newDepth = curDepth + 1
      constrainedCountW newDepth newPath

type App' a = StateT AppState (ReaderT AppConfig IO) a
runApp' :: App' a -> Int -> IO (a, AppState)
runApp' k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in runReaderT (runStateT k state) config

constrainedCount' :: Int -> FilePath -> App' [(FilePath, Int, Int)]
constrainedCount' curDepth path = do
  contents <- liftIO $ listDirectory $ path
  cfg <- ask
  maxDepth <- get
  rest <- forM contents $ \name -> do
    let newPath = path </> name
    isDir <- liftIO $ doesDirectoryExist newPath
    if isDir && curDepth < cfgMaxDepth cfg
      then do
        let newDepth = curDepth + 1
        st <- get
        when (stDeepestReached st < newDepth) $
          put AppState { stDeepestReached = newDepth }
        constrainedCount' newDepth newPath
      else return []
  return $ (path, length contents, stDeepestReached maxDepth) : concat rest
