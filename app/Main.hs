{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           System.Console.CmdArgs

import           TensorSafe.Commands.Check    (check)
import           TensorSafe.Commands.Compile  (compile)
import           TensorSafe.Commands.Examples (examples)

data TensorSafe = Check   { path :: String }
                | Compile { path :: String, moduleName :: String }
                | Examples
                deriving (Show, Data, Typeable)

cCheck :: TensorSafe
cCheck = Check { path = def &= help "Path to Haskell module with TensorSafe model inside" }

cCompile :: TensorSafe
cCompile = Compile {
    path = def &= help "Path to Haskell module with TensorSafe model inside",
    moduleName = def &= help "The module name inside the TensorSafe model file" }

cExamples :: TensorSafe
cExamples = Examples

tensorSafe :: IO TensorSafe
tensorSafe =
    cmdArgs (modes [cCheck, cCompile, cExamples])
    &= verbosity
    &= help "TensorSafe is a tool to verify Neural Networks models in compilation time"
    &= summary "TensorSafe v0.0.1, (C) Leonardo Pineyro"
    &= details [ "TensorSafe can tell if a Neural Network model is well defined and will run without errors",""
               , "Using a Keras-like API you can define a Neural Network model and TensorSafe will point if there's any error and where"]


main :: IO ()
main = do
    print =<< tensorSafe
    r <- tensorSafe
    case r of
        Check { path = p }                   -> check p
        Compile { path = p, moduleName = m } -> compile p m
        Examples                             -> examples
