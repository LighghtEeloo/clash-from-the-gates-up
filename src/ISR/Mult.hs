{-# LANGUAGE NamedFieldPuns #-}

module ISR.Mult (mult, MultInput, MultOutput) where

import Clash.Prelude

data MultInput = MultInput
  { mcand :: Unsigned 64,
    mplier :: Unsigned 64,
    start :: Bool
  }
  deriving (Generic, NFDataX)

data MultOutput = MultOutput
  { prod :: Unsigned 64,
    done :: Bool
  }
  deriving (Generic, NFDataX)

mult ::
  (HiddenClockResetEnable dom) =>
  Signal dom MultInput ->
  Signal dom MultOutput
mult input =
  ( \MultInput {mcand, mplier, start = _} ->
      MultOutput
        { prod = mcand * mplier,
          done = True
        }
  )
    <$> input
