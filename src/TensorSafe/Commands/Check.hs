module TensorSafe.Commands.Check (check) where

import           Language.Haskell.Interpreter
import           System.Exit

import           TensorSafe.Commands.Utils


check :: String -> IO ()
check path = do
    r <- runInterpreter $ loadModules [path]
    case r of
        Left err -> do
            putStrLn $ errorString err
            exitWith $ ExitFailure 1
        Right () -> do
            putStrLn $ "Ok"
            exitWith $ ExitSuccess
