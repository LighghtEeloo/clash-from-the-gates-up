module ISR.ISR where

import Clash.Prelude hiding (init)
import Control.Lens

-- A sieve for the highest bit
-- sievePure :: BitVector 16 -> Maybe (BitVector 4)
-- sievePure regs =
--   ifoldr sel Nothing xs
--   where
--     sel _ _ (Just a) = Just a
--     sel i x Nothing = if x == 1 then Just (pack i) else Nothing
--     xs = bv2v regs

-- sieve :: "reg_status" ::: Signal dom (BitVector 16) -> "selection" ::: Signal dom (Maybe (BitVector 4))
-- sieve = fmap sievePure

data Input = Input
  { _value :: Unsigned 64,
    _reset :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data Output = Output
  { _result_o :: "result" ::: Unsigned 32,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

data State = State
  { _start :: "start" ::: Bool,
    _valueSaved :: "value_saved" ::: Unsigned 64,
    _mask :: "mask" ::: BitVector 32,
    _result :: "result" ::: Unsigned 32
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

trans :: State -> Input -> (State, Output)
trans stateLast input =
  (state, output)
  where
    proposal = state ^. result .|. unpack (state ^. mask)
    state =
      stateLast
        & mask %~ (`shiftR` 1)
    output =
      Output
        (stateLast ^. result)
        (stateLast ^. mask == 0)

init :: State
init = State False 0 0 0

isr :: (HiddenClockResetEnable dom) => "input" ::: Signal dom Input -> "output" ::: Signal dom Output
isr = mealy trans init
