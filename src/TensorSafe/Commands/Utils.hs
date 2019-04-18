{-| This module provides simple IO functions to operate with command line programs. -}
module TensorSafe.Commands.Utils (
    errorString,
    say
) where

import           Data.List
import           Language.Haskell.Interpreter

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
