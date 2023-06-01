-- {-# LANGUAGE NamedFieldPuns, RecordWildCards, DuplicateRecordFields #-}

module ISR.StagedMult (staged, Input, Output, ProdBundle) where

import Clash.Prelude hiding (init)
import Control.Lens hiding (op)

-- import Control.Monad.RWS
-- import Data.Monoid.Generic

data ProdBundle = ProdBundle
  { _prod :: Unsigned 64,
    _mplier :: Unsigned 64,
    _mcand :: Unsigned 64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''ProdBundle

data Input = Input
  { _start :: Bool,
    _prods_in :: ProdBundle
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output = Output
  { _prods_out :: ProdBundle,
    _done :: Bool
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

staged :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
staged = moore trans output init
  where
    trans (s :: State) (i :: Input) =
      State
        { _prod_s =
            ProdBundle
              { _prod = i ^. (prods_in . prod),
                _mplier = i ^. (prods_in . mplier) `shiftR` 8,
                _mcand = i ^. (prods_in . mcand) `shiftL` 8
              },
          _partial = s ^. partial,
          _done_s = i ^. start
        }
    output (s :: State) =
      Output
        { _prods_out =
            ProdBundle
              { _prod = s ^. partial + s ^. (prod_s . prod),
                _mplier = s ^. (prod_s . mplier),
                _mcand = s ^. (prod_s . mcand)
              },
          _done = s ^. done_s
        }
    init :: State =
      State
        { _prod_s = ProdBundle 0 0 0,
          _partial = 0,
          _done_s = False
        }
