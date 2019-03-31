module TensorSafe.Commands.Examples where

import           TensorSafe.Examples.Examples

examples :: IO ()
examples = do
    genericTensorExample
    putStrLn $ "\n\n"
    genericTensorExample2
    putStrLn $ "\n\n"
    simpleExample
    putStrLn $ "\n\n"
    mnistExample
    putStrLn $ "\n\n"
    mnistExampleDense

