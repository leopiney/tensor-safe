{-# LANGUAGE OverloadedStrings #-}
module TensorSafe.Compile.Expr where

import           Data.Map
import           Data.Text.Lazy as T
import           Formatting


data CNetwork = CNSequence CNetwork
              | CNCons CNetwork CNetwork
              | CNLayer String (Map String String)
              | CNReturn
              | CNNil
              deriving Show

data Backend = JavaScript | Python deriving Show

evalCNetwork :: Backend -> CNetwork -> Text
evalCNetwork Python cn     = T.intercalate "\n" (evalPython cn)
evalCNetwork JavaScript cn = T.intercalate "\n" (evalJS cn)


evalPython :: CNetwork -> [Text]
evalPython = undefined

evalJS :: CNetwork -> [Text]
evalJS (CNSequence cn)  = ["const model = tf.sequential();"] ++ evalJS cn
evalJS (CNCons cn1 cn2) = (evalJS cn1) ++ (evalJS cn2)
evalJS (CNNil) = []
evalJS CNReturn = [] -- ["return model"]
evalJS (CNLayer layer params) =
    [format ("model.add(tf.layers." % string % "(" % string % "))") layer (paramsToJS params)]


paramsToJS :: Map String String -> String
paramsToJS m =
    (foldrWithKey showParam "{ " m) ++ "}"
    where
        showParam :: String -> String -> String -> String
        showParam key value accum = accum ++ key ++ ": " ++ value ++ ", "
