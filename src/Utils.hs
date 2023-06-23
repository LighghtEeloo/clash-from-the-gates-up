module Utils (monomorphizeEntity, (|>)) where

import Clash.Prelude

monomorphizeEntity ::
  (KnownDomain dom) =>
  ((HiddenClockResetEnable dom) => r) ->
  Clock dom ->
  Reset dom ->
  Enable dom ->
  r
monomorphizeEntity e clock rst en = withClockResetEnable clock rst en e

(|>) :: a -> (a -> b) -> b
(|>) a f = f a
