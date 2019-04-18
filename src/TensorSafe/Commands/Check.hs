{-| This module provides checking and interpretation functions using the "hint" library. -}
module TensorSafe.Commands.Check (check) where

import           Language.Haskell.Interpreter
import           System.Exit

import           TensorSafe.Commands.Utils

-- | Checks if the file at the specified path compiles successfully.
check :: String -> IO ()
check path = do
    r <- runInterpreter $ loadModules [path]
    case r of
        Left err -> do
            putStrLn $ errorString err
            exitWith $ ExitFailure 1
        Right () -> do
            exitWith $ ExitSuccess
