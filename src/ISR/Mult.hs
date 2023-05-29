{-# LANGUAGE NamedFieldPuns #-}

module ISR.Mult (mult, Input, Output) where

import Clash.Prelude

data Input = Input
  { mcand :: Unsigned 64,
    mplier :: Unsigned 64,
    start :: Bool
  }
  deriving (Generic, NFDataX)

data Output = Output
  { prod :: Unsigned 64,
    done :: Bool
  }
  deriving (Generic, NFDataX)

multPure :: Input -> Output
multPure Input {mcand, mplier, start = _} =
  Output
    { prod = mcand * mplier,
      done = True
    }

mult ::
  (HiddenClockResetEnable dom) =>
  Signal dom Input ->
  Signal dom Output
mult = fmap multPure
