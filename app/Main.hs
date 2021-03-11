{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import System.Console.CmdArgs
import TensorSafe.Commands.Check (check)
import TensorSafe.Commands.Compile (compile)
import TensorSafe.Commands.Examples (examples)

data Backend = JavaScript | Python deriving (Data, Eq, Show, Typeable)

data TensorSafe
  = Check {path :: FilePath}
  | Compile
      { path :: FilePath,
        module_name :: String,
        backend :: Backend,
        out :: Maybe FilePath
      }
  | Examples
  deriving (Data, Eq, Show, Typeable)

cCheck :: TensorSafe
cCheck =
  Check
    { path = def &= typ "PATH" &= help "Path to Haskell module with TensorSafe model inside"
    }
    &= help "Checks if a Neural Network model is valid or not"

cCompile :: TensorSafe
cCompile =
  Compile
    { path = def &= typ "PATH" &= help "Path to Haskell module with TensorSafe model inside",
      module_name = def &= help "The module name inside the TensorSafe model file",
      backend =
        enum
          [ JavaScript &= help "Compile to JavaScript backend",
            Python &= help "Compile to Python backend"
          ],
      out = def &= help "If specified, the output file path to which the network will be generated"
    }
    &= help "Compiles module and outputs Neural Network model for the specified backend"

cExamples :: TensorSafe
cExamples = Examples &= help "Show some examples"

tensorSafe :: IO TensorSafe
tensorSafe = cmdArgs (modes [cCompile, cCheck, cExamples])

main :: IO ()
main = do
  -- print =<< tensorSafe
  r <- tensorSafe
  case r of
    Check {path = p} -> check p
    Compile {path = p, module_name = m, backend = b, out = o} -> compile p m (show b) o
    Examples -> examples
