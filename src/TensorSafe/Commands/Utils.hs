module TensorSafe.Commands.Utils where

import           Data.List
import           Language.Haskell.Interpreter


errorString :: InterpreterError -> String
errorString (WontCompile es) =
    intercalate "\n" (header : map unbox es)
    where
        header = "ERROR: Won't compile:"
        unbox (GhcError e) = e
errorString e = show e


say :: String -> Interpreter ()
say = liftIO . putStrLn
