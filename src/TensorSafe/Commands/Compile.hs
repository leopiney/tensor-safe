module TensorSafe.Commands.Compile (compile) where

import           Language.Haskell.Interpreter
import           System.Exit

import           TensorSafe.Commands.Utils


compile :: String -> String -> String -> Maybe FilePath -> IO ()
compile path moduleName backend out = do
    r <- runInterpreter $ checkAndCompile path moduleName backend out
    case r of
        Left err -> do
            putStrLn $ errorString err
            exitWith $ ExitFailure 1
        Right () -> do
            exitWith $ ExitSuccess

checkAndCompile :: String -> String -> String -> Maybe FilePath -> Interpreter ()
checkAndCompile path moduleName backend out = do
    loadModules [path]
    setTopLevelModules [moduleName]
    setImportsQ [("TensorSafe", Nothing), ("Data.Text.Lazy", Nothing)]

    case out of
        Nothing -> do
            r <- interpret ("unpack $ generate " ++ backend ++ " (toCNetwork nn)") (as :: String)
            liftIO $ putStrLn r
        Just f -> do
            r <- interpret ("unpack $ generate " ++ backend ++ " (toCNetwork nn)") (as :: String)
            liftIO $ writeFile f r
