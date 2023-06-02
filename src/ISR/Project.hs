module ISR.Project (topEntity) where

import Clash.Annotations.TH
import Clash.Prelude
import qualified ISR.Mult as Mult

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  "input" ::: Signal System Mult.Input ->
  "output" ::: Signal System Mult.Output
topEntity clock reset en input =
  exposeClockResetEnable (Mult.mult reset input) clock reset en

makeTopEntityWithName 'topEntity "ISR"
