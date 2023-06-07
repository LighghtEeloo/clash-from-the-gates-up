module Q.Project () where

import Clash.Annotations.TH (makeTopEntityWithName)
import Clash.Prelude
import Control.Lens hiding (Index, imap, indices)
import Utils (monomorphizeEntity)

type Arity n = Index (n + 1)

data Slice n t = Slice
  { _entries :: Vec n t,
    _len :: Arity n
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Slice

transfer ::
  forall outCap inCap cap a.
  ( KnownNat outCap,
    KnownNat inCap,
    KnownNat cap,
    NFDataX a,
    outCap <= cap,
    inCap <= cap
  ) =>
  Slice cap a ->
  (Arity outCap, Slice inCap a) ->
  Slice cap a
transfer state (pop, influx) =
  state
    & entries .~ map trans indicesI
    & len .~ len'
  where
    afterPop = (state ^. len) - resize pop
    len' = afterPop + resize (influx ^. len)
    trans i | resize i < afterPop = (state ^. entries) !! (i + resize pop)
    trans i | resize i >= len' = undefined
    trans i = (influx ^. entries) !! (resize i - afterPop)

outbound ::
  forall outCap inCap cap a.
  ( KnownNat outCap,
    KnownNat inCap,
    KnownNat cap,
    NFDataX a,
    outCap <= cap,
    inCap <= cap
  ) =>
  Slice (outCap + (cap - outCap)) a ->
  (Slice outCap a, Arity inCap)
outbound state = (efflux, push)
  where
    len' = state ^. len
    efflux = Slice (takeI (state ^. entries)) (min (natToNum @outCap) (resize len'))
    push = resize ((natToNum @cap) - len')

entity ::
  forall dom outCap inCap cap a.
  ( HiddenClockResetEnable dom,
    KnownNat outCap,
    KnownNat inCap,
    KnownNat cap,
    NFDataX a,
    outCap <= cap,
    inCap <= cap
  ) =>
  Slice cap a ->
  Signal dom (Arity outCap, Slice inCap a) ->
  Signal dom (Slice outCap a, Arity inCap)
entity = moore transfer outbound

type Data = BitVector 64

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System ("pop" ::: Arity 2, "influx" ::: Slice 4 Data) ->
  Signal System ("efflux" ::: Slice 2 Data, "push" ::: Arity 4)
topEntity =
  monomorphizeEntity $ entity $ Slice entries_ 0
  where
    entries_ = map (const undefined) (indices d8)

makeTopEntityWithName 'topEntity "Queue"
