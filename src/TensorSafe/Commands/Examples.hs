{-| This module implements the examples command for TensorSafe. -}
module TensorSafe.Commands.Examples (examples) where

import           TensorSafe.Examples.Examples

-- | Outputs to stdout the results of the examples
examples :: IO ()
examples = do
    simpleExample
    putStrLn $ "\n\n"
    mnistExample
    putStrLn $ "\n\n"
    mnistExampleDense

