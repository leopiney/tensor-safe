{-# LANGUAGE GADTs #-}

module TensorSafe.Compiler (
    compileNetwork
) where

import           TensorSafe.Layers  (compile)
import           TensorSafe.Network

compileNetwork' :: Network xs ss -> String
compileNetwork' NNil      = ""
compileNetwork' (l :~~ n) = compile l ++ "\n  " ++ compileNetwork' n

compileNetwork :: Network xs ss -> String
compileNetwork n =
    "import * as tf from '@tensorflow/tfjs';\n\
    \\n\
    \\n\
    \ function createModel() {\n\
    \  const model = tf.sequential();\n\
    \\n\
    \  " ++ compileNetwork' n ++ "\n\
    \  return model\n\
    \}\n"
