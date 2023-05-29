module Example.Project (topEntity) where

import Clash.Prelude
import Example.DFF

-- | 'topEntity' is Clash's equivalent of 'main' in other programming
-- languages. Clash will look for it when compiling 'Example.Project'
-- and translate it to HDL. While polymorphism can be used freely in
-- Clash projects, a 'topEntity' must be monomorphic and must use non-
-- recursive types. Or, to put it hand-wavily, a 'topEntity' must be
-- translatable to a static number of wires.
topEntity ::
  "clock"  ::: Clock System ->
  "reset"  ::: Reset System ->
  "enable" ::: Enable System ->
  "drst"   ::: Signal System Bool ->
  Signal System (Signed 8) ->
  Signal System (Signed 8) ->
  Signal System (Signed 8)
topEntity clock reset en drst d a =
  exposeClockResetEnable (dff (0 :: Signed 8) drst d a) clock reset en
