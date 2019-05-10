{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs     #-}
module ModelModule where
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
        Flatten,
        Dense 784 42,
        Relu,
        Dense 42 10,
        Sigmoid
    ]
    ('D3 28 28 1)  -- Input
    ('D1 10)       -- Output
  
    -- { END NETWORK }

nn :: NN
nn = mkINetwork
-- { END }

-- | End of block
--   don't remove this comment
--
