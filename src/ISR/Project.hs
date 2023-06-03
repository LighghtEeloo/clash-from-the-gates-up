{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module ISR.Project (topEntity) where

import Clash.Annotations.TH (makeTopEntityWithName)
import Clash.Prelude hiding (init)
import Control.Lens
import qualified Mult.Project as Mult
import Utils (monomorphizeEntity)

data Input = Input
  { _value :: "value" ::: Unsigned 64,
    _reset :: "rst" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output = Output
  { _result_o :: "result" ::: Unsigned 32,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

-- makeLenses ''Output

data State = State
  { _start :: "start" ::: Bool,
    _valueSaved :: "value_saved" ::: Unsigned 64,
    _mask :: "mask" ::: BitVector 32,
    _result :: "result" ::: Unsigned 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

trans :: State -> (Input, Mult.Output) -> (State, (Output, Mult.Input))
trans stateLast (input, multOut) =
  (state, (output, multIn))
  where
    proposal = state ^. result .|. unpack (state ^. mask)
    state =
      if input ^. reset
        then init
        else case undefined of
          LT ->
            stateLast
              & mask %~ (`shiftR` 1)
          GT ->
            undefined
          EQ ->
            undefined
    output =
      Output
        (stateLast ^. result)
        (stateLast ^. mask == 0)
    multIn = undefined

init :: State
init = State False 0 0 0

entity :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
entity input = output
  where
    (output, multIn) = isr (input, multOut)
    isr = mealyB trans init
    multOut = Mult.entity multIn

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = monomorphizeEntity entity

makeTopEntityWithName 'topEntity "isr"
