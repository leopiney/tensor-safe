module TensorSafe.Compile.Expr where

import           Data.Map

data CNetwork = CNSequence CNetwork
              | CNCons CNetwork CNetwork
              | CNLayer String (Map String String)
              | CNReturn
              deriving Show
