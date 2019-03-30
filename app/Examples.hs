module Examples where

import           TensorSafe.Examples.Examples

main :: IO ()
main = do
    genericTensorExample
    putStrLn $ "\n\n"
    genericTensorExample2
    putStrLn $ "\n\n"
    simpleExample
    putStrLn $ "\n\n"
    mnistExample
    putStrLn $ "\n\n"
    mnistExampleDense
