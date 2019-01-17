{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorSafe
    ( someFunc
    ) where

import           Data.Maybe        (Maybe, fromJust)
import           TensorSafe.Shape
import           TensorSafe.Tensor

someFunc :: IO ()
someFunc =
    let
        (shape1 :: Maybe (Shape '[1,3])) = fromUnsafe (UnsafeShape [1,3])
        (shape2bad :: Maybe (Shape '[3,1])) = fromUnsafe (UnsafeShape [3,3])
        (shape2 :: Maybe (Shape '[3,4])) = fromUnsafe (UnsafeShape [3,4])
        (shape3 :: Maybe (Shape '[28,28])) = fromUnsafe (UnsafeShape [28,28])
        (tensor1 :: Tensor Bool '[28, 28]) = Tensor DT_BOOL (fromJust shape3)
        (tensor2 :: Tensor Bool '[28, 28]) = Tensor DT_BOOL (fromJust shape3)
        (tensor3 :: Tensor Bool '[28, 28]) = tensor1 `add` tensor2
        (tensor4 :: Tensor Float '[1, 3]) = Tensor DT_FLOAT (fromJust shape1)
        (tensor5 :: Tensor Float '[3, 4]) = Tensor DT_FLOAT (fromJust shape2)
        (tensor6 :: Tensor Float '[1, 4]) = tensor4 `matMult` tensor5
        in
        do
            putStrLn $ "Building Shape[1,3] from unsafe list [1,3]: " ++ show shape1
            putStrLn $ "Building Shape[3,1] from unsafe list [3,3]: " ++ show shape2bad
            putStrLn $ "Building Shape[3,4] from unsafe list [3,4]: " ++ show shape2
            putStrLn $ "Building Shape[28,28] from unsafe list [28,28]: " ++ show shape3
            putStrLn $ "Building Tensor [BOOL] [28,28] from unsafe list [28,28]: " ++ show tensor1
            putStrLn $ "Building Tensor [BOOL] [28,28] from unsafe list [28,28]: " ++ show tensor2
            putStrLn $ "Building Tensor [BOOL] [28,28] by adding two tensors of [28,28]: " ++ show tensor3
            putStrLn $ "Building Tensor [FLOAT] [1, 4] by multiplying two tensors of [1,3] and [3,4]: " ++ show tensor6
