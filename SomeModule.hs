{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module SomeModule where
import           Data.Text.Lazy    (unpack)

import           TensorSafe
import           TensorSafe.Layers
import           TensorSafe.Shape


-- | Initial block of code to fill here
--   don't remove this comment
--

-- { BEGIN }
type NN = MkINetwork
    -- { BEGIN NETWORK }
    '[
        Conv2D 1 16 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 16 32 3 3 1 1,
        Relu,
        MaxPooling 2 2 2 2,
        Conv2D 32 32 3 3 1 1,
        Relu,
        Flatten,
        Dense 288 64,
        Sigmoid,
        Dense 64 10,
        Sigmoid
    ]
    ('D3 28 28 1)
    ('D1 10)
    -- { END NETWORK }

nn :: NN
nn = mkINetwork
-- { END }

-- | End of block
--   don't remove this comment
--
