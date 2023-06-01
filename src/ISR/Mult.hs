module ISR.Mult (mult, Input, Output) where

import Clash.Prelude hiding (init)
-- import Control.Lens
import qualified ISR.StagedMult as Staged

data Input = Input
  { _mcand :: "mcand" ::: Unsigned 64,
    _mplier :: "mplier" ::: Unsigned 64,
    _start :: "start" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data Output = Output
  { _prod :: "prod" ::: Unsigned 64,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

mult :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
mult input = output
  where
    (mcand, mplier, start) = (_mcand <$> input, _mplier <$> input, _start <$> input)
    init = Staged.Output <$> prod <*> start
      where
        prod = Staged.ProdBundle <$> partial <*> mplier <*> mcand
        partial = pure 0
    wire out () =
      Staged.staged stagedInput
      where
        stagedInput =
          Staged.Input <$> (Staged._done <$> out) <*> (Staged._prods_out <$> out)
    stagedOut = foldl wire init $ map (const ()) (indices d8)
    output =
      Output
        <$> (Staged._prod . Staged._prods_out <$> stagedOut)
        <*> (Staged._done <$> stagedOut)
