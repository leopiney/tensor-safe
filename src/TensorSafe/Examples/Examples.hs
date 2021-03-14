{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module wraps all examples in simple fuctions.
module TensorSafe.Examples.Examples
  ( mnistExample,
    mnistExampleDense,
    simpleExample,
    mnistConcatenateExample,
    mnistConcatenateComplexExample,
  )
where

import Data.Text.Lazy (unpack)
import TensorSafe.Compile.Expr (JavaScript (..), generate)
import TensorSafe.Examples.ConcatenateExample (mnistConcatenate, mnistConcatenateComplex)
import TensorSafe.Examples.MnistExample (mnist, mnistDense)
import TensorSafe.Examples.SimpleExample
  ( lstm,
    myNet,
    myNet2,
    myNet3,
  )
import TensorSafe.Network (toCNetwork)

-- | Puts simple examples results to stdout
simpleExample :: IO ()
simpleExample =
  do
    putStrLn "Simple network example"
    putStrLn "----------------------"
    print myNet
    putStrLn "Simple network example"
    putStrLn "----------------------"
    print myNet2
    putStrLn "Simple network example"
    putStrLn "----------------------"
    print myNet3
    putStrLn "Simple LSTM network example"
    putStrLn "----------------------"
    print lstm

-- | Puts MNIST examples results to stdout
mnistExample :: IO ()
mnistExample =
  do
    putStrLn "MNIST example"
    putStrLn "-------------"
    print mnist
    putStrLn "\n"
    putStrLn "MNIST compilation"
    putStrLn "-------------"
    print (toCNetwork mnist)
    putStrLn "\n"
    putStrLn "MNIST generation"
    putStrLn "-------------"
    putStrLn $ unpack $ generate JavaScript (toCNetwork mnist)

-- | Puts MNIST Dense examples results to stdout
mnistExampleDense :: IO ()
mnistExampleDense =
  do
    putStrLn "MNIST Dense example"
    putStrLn "-------------"
    print mnistDense
    putStrLn "\n"
    putStrLn "MNIST compilation"
    putStrLn "-------------"
    print (toCNetwork mnistDense)
    putStrLn "\n"
    putStrLn "MNIST generation"
    putStrLn "-------------"
    putStrLn $ unpack $ generate JavaScript (toCNetwork mnistDense)

mnistConcatenateExample :: IO ()
mnistConcatenateExample =
  do
    putStrLn "MNIST Concatenate with Concatenate example"
    putStrLn "------------------------------"
    print mnistConcatenate
    putStrLn "\n"
    putStrLn "MNIST Concatenate compilation"
    putStrLn "------------------------------"
    print (toCNetwork mnistConcatenate)
    putStrLn "\n"
    putStrLn "MNIST Concatenate generation"
    putStrLn "------------------------------"
    putStrLn $ unpack $ generate JavaScript (toCNetwork mnistConcatenate)

mnistConcatenateComplexExample :: IO ()
mnistConcatenateComplexExample =
  do
    putStrLn "MNIST Concatenate Complex with Concatenate example"
    putStrLn "------------------------------"
    print mnistConcatenateComplex
    putStrLn "\n"
    putStrLn "MNIST Concatenate Complex compilation"
    putStrLn "------------------------------"
    print (toCNetwork mnistConcatenateComplex)
    putStrLn "\n"
    putStrLn "MNIST Concatenate Complex generation"
    putStrLn "------------------------------"
    putStrLn $ unpack $ generate JavaScript (toCNetwork mnistConcatenateComplex)