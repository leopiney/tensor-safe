-- | This module provides simple IO functions to operate with command line programs.
module TensorSafe.Commands.Utils
  ( errorString,
    say,
  )
where

import Data.List (intercalate)
import Language.Haskell.Interpreter
  ( GhcError (GhcError),
    Interpreter,
    InterpreterError (WontCompile),
    MonadIO (liftIO),
  )

-- | Transforms an InterpreterError into a string.
errorString :: InterpreterError -> String
errorString (WontCompile es) =
  intercalate "\n" (header : map unbox es)
  where
    header = "Compilation error:"
    unbox (GhcError e) = e
errorString e = show e

-- | Lifts putStrLn to the Interpreter.
say :: String -> Interpreter ()
say = liftIO . putStrLn
