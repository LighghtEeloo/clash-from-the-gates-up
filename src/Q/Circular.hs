module Q.Circular where

import Clash.Prelude hiding (init, head)
import Control.Lens hiding (Index, indices, ifoldl, imap, _head)

type Arity n = Index (n + 1)

type View n a = (Vec n a, Arity n)

data State cap t = State
  { _vec :: Vec cap t,
    _head :: Index cap,
    _size :: Arity cap
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''State

type StateTransfer pushN popN cap t = State cap t -> (Arity popN, View pushN t) -> State cap t

transfer :: forall pushN popN cap t . (KnownNat pushN, KnownNat popN, KnownNat cap, pushN <= cap, popN <= cap, NFDataX t) => StateTransfer pushN popN cap t
transfer current (popArity, pushView) =
  current
    & head %~ (\h -> wrap (h + resize popArity))
    & size %~ (\s -> s + resize (snd pushView) - resize popArity)
    & vec %~ imap (\_i _a -> undefined)
  where
    capacity = natToNum @cap
    wrap i = if i < capacity then i else i - capacity

    -- poppedVec = ifoldl (\acc i a -> if resize i < popArity then acc else acc <<+ a) (current ^. vec) (current ^. vec)
    -- vec =
    -- tail = if vtail < natToNum @cap then vtail else vtail - natToNum @cap

-- makeLenses ''State

-- pushUnsafe :: forall n pro a . (KnownNat n, KnownNat pro, NFDataX a) => View pro a -> State n a -> State n a
-- pushUnsafe (_xs, len) state | state ^. size + resize len > natToNum @n = undefined
-- pushUnsafe (xs, len) state = state
