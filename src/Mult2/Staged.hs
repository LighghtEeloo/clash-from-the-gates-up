module Mult2.Staged () where

import Clash.Annotations.TH (makeTopEntityWithName)
import Clash.Prelude hiding (init)
import Control.Lens (makeLenses, (%~), (&), (.~), (^.))
import Utils (monomorphizeEntity)

data ProdBundle = ProdBundle
  { _prod :: "prod" ::: Unsigned 64,
    _mplier :: "mplier" ::: Unsigned 64,
    _mcand :: "mcand" ::: Unsigned 64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''ProdBundle

times :: ProdBundle -> Unsigned 64
times p = mcand' * extend mplier'
  where
    mcand' = p ^. mcand
    mplier' = (truncateB $ p ^. mplier) :: Unsigned 32

shiftProd :: ProdBundle -> ProdBundle
shiftProd pd =
  pd
    & mplier %~ (`shiftR` 32)
    & mcand %~ (`shiftL` 32)

data Input = Input
  { _start :: "start" ::: Bool,
    _prodsIn :: "in" ::: ProdBundle
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output = Output
  { _prodsOut :: "out" ::: ProdBundle,
    _done :: "done" ::: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

-- makeLenses ''Output

data State = State
  { _prodState :: ProdBundle,
    _partial :: Unsigned 64,
    _doneState :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

trans :: State -> Input -> State
trans state input =
  state
    & prodState .~ shiftProd (input ^. prodsIn)
    & partial .~ times (input ^. prodsIn)
    & doneState .~ input ^. start

out :: State -> Output
out state = Output po doneOut
  where
    po = state ^. prodState & prod .~ (state ^. partial + state ^. (prodState . prod))
    doneOut = state ^. doneState

init :: State
init =
  State
    (ProdBundle 0 0 0)
    0
    False

entity :: (HiddenClockResetEnable dom) => Signal dom Input -> Signal dom Output
entity = moore trans out init

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System Input ->
  Signal System Output
topEntity = monomorphizeEntity entity

makeTopEntityWithName 'topEntity "mult_staged"
