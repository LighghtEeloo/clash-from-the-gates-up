module ISR.Mult (mult, Input, Output) where

import Clash.Prelude hiding (init)
import Control.Lens hiding (indices)
import qualified ISR.StagedMult as Staged

data Input = Input
  { _mcand :: "mcand" ::: Unsigned 64,
    _mplier :: "mplier" ::: Unsigned 64,
    _start :: "start" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output = Output
  { _prod :: "prod" ::: Unsigned 64,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

-- makeLenses ''Output

mult :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
mult input = output
  where
    init = Staged.Output <$> prod_init <*> (view start <$> input)
      where
        prod_init =
          Staged.ProdBundle
            <$> partial
            <*> (view mplier <$> input)
            <*> (view mcand <$> input)
        partial = pure 0
    wire out () =
      Staged.staged stagedInput
      where
        stagedInput =
          Staged.Input
            <$> (view Staged.done <$> out)
            <*> (view Staged.prods_out <$> out)
    stagedOut = foldl wire init $ map (const ()) (indices d8)
    output =
      Output
        <$> (view (Staged.prods_out . Staged.prod) <$> stagedOut)
        -- <*> (stagedOut ^. to (fmap (view Staged.done)))
        -- <*> (stagedOut & fmap (^. Staged.done))
        <*> (view Staged.done <$> stagedOut)
