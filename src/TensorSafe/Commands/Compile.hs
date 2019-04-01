module TensorSafe.Commands.Compile (compile) where

import           Language.Haskell.Interpreter
import           System.Exit

import           TensorSafe.Commands.Utils


compile :: String -> String -> IO ()
compile path moduleName = do
    r <- runInterpreter $ checkAndCompile path moduleName
    case r of
        Left err -> do
            putStrLn $ errorString err
            exitWith $ ExitFailure 1
        Right () -> do
            exitWith $ ExitSuccess

checkAndCompile :: String -> String -> Interpreter ()
checkAndCompile path moduleName = do
    loadModules [path]
    setTopLevelModules [moduleName]
    setImportsQ [("TensorSafe", Nothing), ("Data.Text.Lazy", Nothing)]

    r <- interpret "unpack $ evalCNetwork JavaScript (toCNetwork nn)" (as :: String)
    liftIO $ putStrLn r
