{-# LANGUAGE NamedFieldPuns #-}

module ISR.StagedMult (staged, Input, Output, ProdBundle) where

import Clash.Prelude

data ProdBundle = ProdBundle
  { prod :: Unsigned 64,
    mplier :: Unsigned 64,
    mcand :: Unsigned 64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data Input = Input
  { start :: Bool,
    prods_in :: ProdBundle
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data Output = Output
  { prods_out :: ProdBundle,
    done :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

stagedPure :: Input -> Output
stagedPure Input {start, prods_in = ProdBundle {prod, mplier, mcand}} =
  Output
    { prods_out =
        ProdBundle
          { prod = partial + prod,
            mplier = mplier `shiftR` width,
            mcand = mcand `shiftL` width
          },
      done =
        start
    }
  where
    width = 8
    partial = ((unpack $ zeroBits ++# slice d7 d0 mplier) :: Unsigned 64) * mcand

staged :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
staged = fmap stagedPure
