module Utils (monomorphizeEntity, mealy2moore, moore2mealy) where

import Clash.Prelude

monomorphizeEntity ::
  (KnownDomain dom) =>
  ((HiddenClockResetEnable dom) => r) ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  r
monomorphizeEntity e clock rst en = withClockResetEnable clock rst en e

mealy2moore ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  ((s -> i -> (s, o)) -> s -> Signal dom i -> Signal dom o) ->
  ((s -> i -> s) -> (s -> o) -> s -> Signal dom i -> Signal dom o)
mealy2moore mealyEntity trans out = mealyEntity (\s i -> (trans s i, out s))

moore2mealy ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  (((s, i) -> i -> (s, i)) -> ((s, i) -> o) -> (s, i) -> Signal dom i -> Signal dom o) ->
  ((s -> i -> (s, o)) -> (s, i) -> Signal dom i -> Signal dom o)
moore2mealy mooreEntity trans =
  mooreEntity
    (\(s, i) i' -> (fst $ trans s i, i'))
    (\(s, i) -> snd $ trans s i)
