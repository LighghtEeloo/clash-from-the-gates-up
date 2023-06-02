module ISR.StagedMult where

import Clash.Prelude hiding (init)
import Control.Lens hiding (op)

-- import Control.Monad.RWS
-- import Data.Monoid.Generic

data ProdBundle = ProdBundle
  { _prod :: "prod" ::: Unsigned 64,
    _mplier :: "mplier" ::: Unsigned 64,
    _mcand :: "mcand" ::: Unsigned 64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''ProdBundle

data Input = Input
  { _start :: "start" ::: Bool,
    _prods_in :: "prod" ::: ProdBundle
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output = Output
  { _prods_out :: "prod" ::: ProdBundle,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Output

data State = State
  { _prod_s :: ProdBundle,
    _partial :: Unsigned 64,
    _done_s :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

trans :: State -> Input -> State
trans state input =
  state
    & prod_s
      .~ ( input ^. prods_in
             & mplier %~ (`shiftR` 8)
             & mcand %~ (`shiftL` 8)
         )
    & done_s
      .~ input ^. start

output :: State -> Output
output state =
  Output
    ( state ^. prod_s
        & prod %~ (+ state ^. partial)
    )
    (state ^. done_s)

init :: State
init =
  State
    (ProdBundle 0 0 0)
    0
    False

staged :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
staged = moore trans output init
