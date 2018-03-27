module Main where

import System.Daemon
import Control.Pipe.Socket (Handler)
import Control.Pipe.C3 (commandReceiver)
import System.Exit
import Control.Monad (when)
import System.Environment (getArgs, lookupEnv)
import Data.Default (def)
import Data.Maybe (maybe, catMaybes)
import qualified Data.Map as M

import Common
import Vpn
import AptUpdate

-- `when` wrapped in the Maybe monad (passing Nothing is equivalent to the condition being false)
maybeWhen :: Monad m => Maybe a -> (a -> Bool) -> m () -> m ()
maybeWhen Nothing _ _ = return ()
maybeWhen (Just x) p a = when (p x) a

main :: IO ()
main = do
    args <- getArgs

    -- If we're root, make sure the daemon's running
    user <- lookupEnv "USER"
    maybeWhen user (== "root") $ do
        ensureDaemonWithHandlerRunning "k-daemon" def daemonProcess
        when (null args) (exitWith ExitSuccess)

    -- Contact the daemon, pass it our commands
    result <- runClient "localhost" (daemonPort def) args :: IO (Maybe Result)
    case result of
        Nothing -> putStrLn "Failed to connect to the daemon" >> exitWith (ExitFailure 1)
        Just r -> case r of
            Failure msg err -> maybe (return ()) putStrLn msg >> exitWith (ExitFailure err)
            Success msg -> maybe (return ()) putStrLn msg


commands :: M.Map String Command
commands = M.fromList $ zipFrom name [aptupdate, vpn]

zipFrom :: (v -> k) -> [v] -> [(k, v)]
zipFrom f xl = zip (map f xl) xl

daemonProcess :: Handler ()
daemonProcess prod cons = do
        sequence_ $ catMaybes $ map startup $ M.elems commands -- Run all startup commands
        commandReceiver daemonInternal prod cons -- Run the default handler

daemonInternal :: [String] -> IO Result
daemonInternal args = do
        case args of
            [] -> retFailure
            (command:args') -> case M.lookup command commands of
                    Nothing -> retFailure
                    Just c  -> handler c $ args'
    where retFailure = return $ Failure (Just "Usage: k-daemon <command> [arguments]") 1