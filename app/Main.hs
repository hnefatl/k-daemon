module Main where

import System.Daemon
import Control.Pipe.Socket (Handler)
import Control.Pipe.C3 (commandReceiver)
import System.Exit
import Control.Monad (when)
import System.Environment (getArgs, lookupEnv)
import Data.Default (def)
import Data.Maybe (maybe, catMaybes)
import Data.IORef
import qualified Data.Map as M

import Common
import Vpn
import AptUpdate
import Sync

-- `when` wrapped in the Maybe monad (passing Nothing is equivalent to the condition being false)
maybeWhen :: Monad m => Maybe a -> (a -> Bool) -> m () -> m ()
maybeWhen Nothing _ _ = return ()
maybeWhen (Just x) p a = when (p x) a

main :: IO ()
main = do
    args <- getArgs

    sync <- makeSync
    let rawCommands = [aptupdate, vpn, sync]
    let commands = M.fromList $ zipFrom name rawCommands

    -- If we're root, make sure the daemon's running
    user <- lookupEnv "USER"
    maybeWhen user (== "root") $ do
        firstRun <- newIORef True
        ensureDaemonWithHandlerRunning "k-daemon" def (daemonProcess firstRun commands)
        when (null args) (exitWith ExitSuccess) -- In the special case of being root and running with no args, exit

    -- Contact the daemon, pass it our commands
    result <- runClient "localhost" (daemonPort def) args :: IO (Maybe Result)
    case result of
        Nothing -> putStrLn "Failed to connect to the daemon" >> exitWith (ExitFailure 1)
        Just r -> case r of
            Failure msg err -> maybe (return ()) putStrLn msg >> exitWith (ExitFailure err)
            Success msg -> maybe (return ()) putStrLn msg


zipFrom :: (v -> k) -> [v] -> [(k, v)]
zipFrom f xl = zip (map f xl) xl

daemonProcess :: IORef Bool -> M.Map String Command -> Handler ()
daemonProcess firstRunRef commands prod cons = do
        -- Only perform startup stuff on the first time the handler's run
        firstRun <- readIORef firstRunRef
        when firstRun $ do
            writeIORef firstRunRef False
            results <- sequence $ catMaybes $ map startup $ M.elems commands -- Run all startup commands
            let failures = filter isFailure results
            when (not $ null failures) $ do { print failures ; exitWith $ ExitFailure 1 }
        commandReceiver (daemonInternal commands) prod cons -- Run the default handler

daemonInternal :: M.Map String Command -> [String] -> IO Result
daemonInternal commands args = do
        case args of
            [] -> retFailure
            (command:args') -> case M.lookup command commands of
                        Nothing -> retFailure
                        Just c  -> handler c $ args'
    where retFailure = return $ Failure (Just "Usage: k-daemon <command> [arguments]") 1