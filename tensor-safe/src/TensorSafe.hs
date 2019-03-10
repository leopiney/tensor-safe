{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorSafe (
    genericTensorExample,
    genericTensorExample2,
    simpleExample,
    mnistExample
) where

import           Data.Maybe                        (Maybe, fromJust)
import           TensorSafe.Compiler               (compileNetwork)
import           TensorSafe.Examples.MnistExample  (mnist)
import           TensorSafe.Examples.SimpleExample (myNet)
import           TensorSafe.Generic.Shape
import           TensorSafe.Generic.Tensor


genericTensorExample :: IO ()
genericTensorExample =
    let
        (shape1 :: Shape [1,3]) = buildShape [1,3]
        (shape2bad :: Maybe (Shape [3,1])) = fromUnsafe (UnsafeShape [3,3])
        (shape2 :: Shape [3,4]) = fromJust $ fromUnsafe (UnsafeShape [3,4])
        (shape3 :: Shape [28,28]) = fromJust $ fromUnsafe (UnsafeShape [28,28])
        (tensor1 :: Tensor Bool [28, 28]) = Tensor DT_BOOL shape3
        (tensor2 :: Tensor Bool [28, 28]) = Tensor DT_BOOL shape3
        (tensor3 :: Tensor Bool [28, 28]) = tensor1 `add` tensor2
        (tensor4 :: Tensor Float [1, 3]) = Tensor DT_FLOAT shape1
        (tensor5 :: Tensor Float [3, 4]) = Tensor DT_FLOAT shape2
        -- (tensor6 :: Tensor Float [1, 6]) = tensor4 `matMult` tensor5 in
        -- Couldn't match type ‘4’ with ‘6’
        -- Expected type: Tensor Float [1, 6]
        --   Actual type: Tensor Float [1, 4]
        (tensor6 :: Tensor Float [1, 4]) = tensor4 `matMult` tensor5 in
        do
            putStrLn $ "Building Shape[1,3] from unsafe list [1,3]: " ++ show shape1
            putStrLn $ "Building Shape[3,1] from unsafe list [3,3]: " ++ show shape2bad
            putStrLn $ "Building Shape[3,4] from unsafe list [3,4]: " ++ show shape2
            putStrLn $ "Building Shape[28,28] from unsafe list [28,28]: " ++ show shape3
            putStrLn $ "Building Tensor [BOOL] [28,28] from unsafe list [28,28]: " ++ show tensor1
            putStrLn $ "Building Tensor [BOOL] [28,28] from unsafe list [28,28]: " ++ show tensor2
            putStrLn $ "Building Tensor [BOOL] [28,28] by adding two tensors of [28,28]: " ++ show tensor3
            putStrLn $ "Building Tensor [FLOAT] [1, 4] by multiplying two tensors of [1,3] and [3,4]: " ++ show tensor6

--
-- The idea here is to define a use case in TF that fails in run time, but here it is guaranteed
-- that the type-checking is going to detect errors in compilation time
--
-- node1 = tf.constant([3.0, 4.0, 5.0], dtype=tf.float32)
-- node2 = tf.constant([4.0, 16.0], dtype=tf.float32)
-- additionNode = tf.add(node1, node2)
--
genericTensorExample2 :: IO ()
genericTensorExample2 =
    let
        node1_shp :: Shape [1, 3] = buildShape [1, 3]
        node1                      = constant DT_FLOAT node1_shp
        node2_shp :: Shape [1, 3] = buildShape [1, 3]
        node2                      = constant DT_FLOAT node2_shp
        node_add                   = node1 `add` node2 in
    do
        putStrLn $ show node_add

--
simpleExample :: IO ()
simpleExample =
    do
        putStrLn $ show myNet

mnistExample :: IO ()
mnistExample =
    do
        putStrLn $ show mnist
        putStrLn $ compileNetwork mnist
