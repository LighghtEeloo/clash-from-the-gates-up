module ISR.Project (topEntity) where

import Clash.Prelude
import ISR.Mult

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System ((Unsigned 64), (Unsigned 64), Bool) ->
  Signal System ((Unsigned 64), Bool)
topEntity clock reset en input = exposeClockResetEnable (bundleOutput (mult $ unbundleInput input)) clock reset en
