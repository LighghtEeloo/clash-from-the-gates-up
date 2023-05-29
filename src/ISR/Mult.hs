{-# LANGUAGE NamedFieldPuns #-}
module ISR.Mult (mult, unbundleInput, bundleOutput) where

import Clash.Prelude

data MultInput dom = MultInput
  { mcand :: Signal dom (Unsigned 64),
    mplier :: Signal dom (Unsigned 64),
    start :: Signal dom Bool
  }

unbundleInput :: Signal dom ((Unsigned 64), (Unsigned 64), Bool) -> MultInput dom
unbundleInput bus =
  MultInput
    { mcand = (\(x, _, _) -> x) <$> bus,
      mplier = (\(_, x, _) -> x) <$> bus,
      start = (\(_, _, x) -> x) <$> bus
    }

data MultOutput dom = MultOutput
  { prod :: Signal dom (Unsigned 64),
    done :: Signal dom Bool
  }

bundleOutput :: MultOutput dom -> Signal dom ((Unsigned 64), Bool)
bundleOutput MultOutput {prod, done} =
  (,) <$> prod <*> done

mult ::
  (HiddenClockResetEnable dom) =>
  MultInput dom ->
  MultOutput dom
mult MultInput {mcand, mplier, start=_} =
  MultOutput
    {
      prod = mcand * mplier,
      done = (pure True)
    }
