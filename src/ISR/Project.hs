module ISR.Project (topEntity) where

import Clash.Prelude
import ISR.Mult

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System MultInput ->
  Signal System MultOutput
topEntity clock reset en input =
  exposeClockResetEnable (mult input) clock reset en
