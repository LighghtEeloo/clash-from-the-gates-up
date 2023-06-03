module DDFF.Project (topEntity) where

import Clash.Annotations.TH
import Clash.Prelude
import DDFF.DFF

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'DDFF.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  "drst" ::: Signal System Bool ->
  "default" ::: Signal System (Signed 8) ->
  "input" ::: Signal System (Signed 8) ->
  "output" ::: Signal System (Signed 8)
topEntity clock reset en drst d a =
  exposeClockResetEnable (dff (0 :: Signed 8) drst d a) clock reset en

makeTopEntityWithName 'topEntity "DffDefault"
