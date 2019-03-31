module Main where

import           System.Environment

import           TensorSafe.Commands.Check    (check)
import           TensorSafe.Commands.Compile  (compile)
import           TensorSafe.Commands.Examples (examples)

main :: IO ()
main = do
    args <- getArgs
    let command = head args
    let commandArgs = tail args
    case command of
        "check"    -> check (commandArgs !! 0)
        "compile"  -> compile (commandArgs !! 0) (commandArgs !! 1)
        "examples" -> examples
        d          -> putStrLn $ "Unknown command " ++ d
