module ISR.Project (topEntity) where

import Clash.Prelude
import qualified ISR.Mult as Mult

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System Mult.Input ->
  Signal System Mult.Output
topEntity clock reset en input =
  exposeClockResetEnable (Mult.mult input) clock reset en
