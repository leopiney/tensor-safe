module Main where

import           TensorSafe

main :: IO ()
main = do
    genericTensorExample
    putStrLn $ "\n\n"
    genericTensorExample2
    putStrLn $ "\n\n"
    simpleExample
    putStrLn $ "\n\n"
    mnistExample
