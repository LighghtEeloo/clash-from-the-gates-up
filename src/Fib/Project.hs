module Fib.Project (topEntity) where

import Clash.Annotations.TH
import Clash.Prelude hiding (init)
import Control.Lens
import Utils (monomorphizeEntity)

data Input = Input
  { _value :: "value" ::: Unsigned 16,
    _reset :: "rst" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data State = State
  { _valueIn :: Unsigned 16,
    _a :: Unsigned 64,
    _b :: Unsigned 64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

newtype Output = Output ("result" ::: Unsigned 64)
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

trans :: State -> Input -> State
trans state input
  | input ^. reset =
      init & valueIn .~ (input ^. value)
  | state ^. valueIn > 1 =
      state
        & a .~ (state ^. b)
        & b +~ (state ^. a)
  | otherwise =
      state

out :: State -> Output
out state
  | state ^. valueIn == 0 =
      Output $ state ^. a
  | otherwise =
      Output $ state ^. b

init :: State
init = State 0 0 1

entity :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
entity = moore trans out init

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = monomorphizeEntity entity

makeTopEntityWithName 'topEntity "fib"
