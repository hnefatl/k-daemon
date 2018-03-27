module Sync
(
    makeSync, runStartup
) where

import Control.Monad (forever)
import Control.Concurrent.Lifted (fork, threadDelay)
import Control.Concurrent.MVar.Lifted
import Control.Monad.Reader
import System.Process (ProcessHandle, createProcess, shell, getProcessExitCode, cwd)
import System.Exit
import System.FilePath

import Common

type SyncReader = ReaderT (MVar ProcessHandle) IO

googleDrivePath :: FilePath
googleDrivePath = "/home/keith/.googledrive"

syncExecName :: FilePath
syncExecName = "grive-sync"

makeSync :: IO Command
makeSync = do
    mHandle <- newEmptyMVar
    return Command
        {
            name = "sync",
            handler = \args -> runReaderT (runSync args) mHandle,
            startup = Just (runReaderT runStartup mHandle)
        }

runSync :: [String] -> SyncReader Result
runSync ("status":_) = getSyncStatus
runSync [] = startSync >> return (Success Nothing)
runSync _ = return $ Failure (Just "Usage: k-daemon sync [status]") 1


getSyncStatus :: SyncReader Result
getSyncStatus = do
    mHandle <- ask
    handle <- readMVar mHandle
    exited <- lift $ getProcessExitCode handle
    case exited of
        Nothing -> return $ Success (Just "syncing") -- A sync process is currently running
        Just exitCode -> case exitCode of
                ExitSuccess -> return $ Success (Just "synced")
                ExitFailure e -> return $ Failure (Just "error") e


-- Start the sync running on startup, store the process handle
runStartup :: SyncReader Result
runStartup = do
    -- Create the syncing process and store the handle
    mHandle <- ask
    handle <- spawnNewSync
    putMVar mHandle handle
    -- On a new thread, start syncing every so often
    _ <- fork syncLoop
    return $ Success Nothing

syncLoop :: SyncReader ()
syncLoop = forever $ do
        startSync
        threadDelay (10 * 60 * 1000000) -- Wait 10 minutes


-- Start a new sync process iff there's not one already running
startSync :: SyncReader ()
startSync = do
        mHandle <- ask
        handle <- takeMVar mHandle
        exited <- lift $ getProcessExitCode handle
        case exited of
            Nothing -> putMVar mHandle handle -- Process hasn't exited yet, put the value back in
            Just _ -> do -- Process has exited
                -- Create a new process, store the new handle
                newHandle <- spawnNewSync
                putMVar mHandle newHandle


-- Spawn a new sync process and return its handle
spawnNewSync :: MonadIO m => m ProcessHandle
spawnNewSync = do
    let shellProcTmp = shell (googleDrivePath </> syncExecName)
    let shellProc = shellProcTmp { cwd = Just googleDrivePath }
    (_, _, _, handle) <- liftIO $ createProcess shellProc
    return handle