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
        (shape1 :: Maybe (Shape '[4,3])) = fromUnsafe (UnsafeShape [4,3])
        (shape2 :: Maybe (Shape '[4,3])) = fromUnsafe (UnsafeShape [4,4])
        (shape3 :: Maybe (Shape '[28,28])) = fromUnsafe (UnsafeShape [28,28])
        (tensor1 :: Tensor Bool '[28, 28]) = Tensor DT_BOOL (fromJust shape3) in
        do
            putStrLn $ "Building Shape[4,3] from unsafe list [4,3]: " ++ show shape1
            putStrLn $ "Building Shape[4,3] from unsafe list [4,4]: " ++ show shape2
            putStrLn $ "Building Shape[28,28] from unsafe list [28,28]: " ++ show shape3
            putStrLn $ "Building Tensor [BOOL] [28,28] from unsafe list [28,28]: " ++ show tensor1
