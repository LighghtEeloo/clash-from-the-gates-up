module Ignite.Project (moore', mealy', moore4mealy, mealy4moore) where

import Clash.Prelude hiding (init)

moore' ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  (s -> i -> s) ->
  (s -> o) ->
  s ->
  (Signal dom i -> Signal dom o)
moore' trans out init input = output
  where
    state = register init state'
    state' = trans <$> state <*> input
    output = out <$> state

mealy' ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  (s -> i -> (s, o)) ->
  s ->
  (Signal dom i -> Signal dom o)
mealy' trans init input = output
  where
    state = register init state'
    (state', output) = unbundle $ trans <$> state <*> input

moore4mealy ::
  (HiddenClockResetEnable dom, NFDataX s) =>
  ((s -> i -> s) -> (s -> o) -> s -> Signal dom i -> Signal dom o)
moore4mealy trans out = mealy (\s i -> (trans s i, out s))

mealy4moore ::
  (HiddenClockResetEnable dom, NFDataX s, NFDataX i) =>
  ((s -> i -> (s, o)) -> (s, i) -> Signal dom i -> Signal dom o)
mealy4moore trans =
  moore
    (\(s, i) i' -> (fst $ trans s i, i'))
    (\(s, i) -> snd $ trans s i)
