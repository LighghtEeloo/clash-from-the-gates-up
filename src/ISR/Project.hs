module ISR.Project (topEntity) where

import Clash.Annotations.TH
import Clash.Prelude
import qualified ISR.Mult as Mult
import Utils (monomorphizeEntity)

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  "input" ::: Signal System Mult.Input ->
  "output" ::: Signal System Mult.Output
topEntity = monomorphizeEntity Mult.entity

makeTopEntityWithName 'topEntity "ISR"
