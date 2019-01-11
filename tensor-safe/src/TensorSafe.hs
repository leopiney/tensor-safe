{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module TensorSafe
    ( someFunc
    ) where

import           Data.Maybe       (Maybe)
import           TensorSafe.Shape

someFunc :: IO ()
someFunc =
    let
        (shape1 :: Maybe (Shape '[4,3])) = fromUnsafe (UnsafeShape [4,3])
        (shape2 :: Maybe (Shape '[4,3])) = fromUnsafe (UnsafeShape [4,4])
        (shape3 :: Maybe (Shape '[4,4])) = fromUnsafe (UnsafeShape [4,4]) in
        do
            putStrLn $ "Building Shape[4,3] from unsafe list [4,3]: " ++ show shape1
            putStrLn $ "Building Shape[4,3] from unsafe list [4,4]: " ++ show shape2
            putStrLn $ "Building Shape[4,4] from unsafe list [4,4]: " ++ show shape3
