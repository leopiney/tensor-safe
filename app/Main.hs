{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Console.CmdArgs

import           TensorSafe.Commands.Check    (check)
import           TensorSafe.Commands.Compile  (compile)
import           TensorSafe.Commands.Examples (examples)

data TensorSafe = Check   { path :: FilePath }
                | Compile { path :: FilePath, module_name :: String }
                | Examples
                deriving (Data, Typeable, Show, Eq)

cCheck :: TensorSafe
cCheck = Check
    { path = def &= typ "PATH" &= help "Path to Haskell module with TensorSafe model inside"
    } &= help "Checks if a Neural Network model is valid or not"

cCompile :: TensorSafe
cCompile = Compile
    { path = def &= typ "PATH" &= help "Path to Haskell module with TensorSafe model inside"
    , module_name = def &= help "The module name inside the TensorSafe model file"
    } &= help "Compiles module and outputs Neural Network model for the specified backend"

cExamples :: TensorSafe
cExamples = Examples &= help "Show some examples"

tensorSafe :: IO TensorSafe
tensorSafe = cmdArgs (modes [cCompile, cCheck, cExamples])

main :: IO ()
main = do
    -- print =<< tensorSafe
    r <- tensorSafe
    case r of
        Check { path = p }                    -> check p
        Compile { path = p, module_name = m } -> compile p m
        Examples                              -> examples
