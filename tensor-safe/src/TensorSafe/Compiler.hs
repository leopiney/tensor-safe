{-# LANGUAGE GADTs #-}

module TensorSafe.Compiler (
    compileNetwork
) where

import           TensorSafe.Layer   (compile)
import           TensorSafe.Network

-- | TODO
compileNetwork :: INetwork xs ss -> String
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

-- | TODO
compileNetwork' :: INetwork xs ss -> String
compileNetwork' INNil     = ""
compileNetwork' (l :~> n) = compile l ++ "\n  " ++ compileNetwork' n
