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
    -- { CONTENT }
    -- { END NETWORK }

nn :: NN
nn = mkINetwork
-- { END }

-- | End of block
--   don't remove this comment
--
