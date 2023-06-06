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
  forall outCap inCap n a.
  (KnownNat outCap, KnownNat inCap, KnownNat n, NFDataX a, inCap <= outCap + n) =>
  Slice (outCap + n) a ->
  (Arity outCap, Slice inCap a) ->
  Slice (outCap + n) a
transfer state (exert, influx) =
  state
    & entries %~ imap elementTrans
    & len .~ len'
  where
    exertedLen = (state ^. len) - resize exert
    len' = exertedLen + resize (influx ^. len)
    elementTrans i = trans
      where
        i' = resize i
        trans _ | i' < exertedLen = (state ^. entries) !! (i + resize exert)
        trans _ | i' >= len' = undefined
        trans _ = (influx ^. entries) !! (i' - exertedLen)

outbound ::
  forall outCap inCap n t.
  (KnownNat outCap, KnownNat inCap, KnownNat n, NFDataX t, inCap <= outCap + n) =>
  Slice (outCap + n) t ->
  (Slice outCap t, Arity inCap)
outbound state = (efflux, insert)
  where
    len' = state ^. len
    efflux = Slice (takeI (state ^. entries)) (min (natToNum @outCap) (resize len'))
    insert = resize ((natToNum @(outCap + n)) - len')

entity ::
  forall dom outCap inCap n t.
  (HiddenClockResetEnable dom, KnownNat outCap, KnownNat inCap, KnownNat n, NFDataX t, inCap <= outCap + n) =>
  Slice (outCap + n) t ->
  Signal dom (Arity outCap, Slice inCap t) ->
  Signal dom (Slice outCap t, Arity inCap)
entity = moore transfer outbound

type Data = BitVector 64

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System ("exert" ::: Arity 2, "influx" ::: Slice 4 Data) ->
  Signal System ("efflux" ::: Slice 2 Data, "insert" ::: Arity 4)
topEntity =
  monomorphizeEntity $ entity $ Slice entries_ 0
  where
    entries_ = map (const undefined) (indices d8)

makeTopEntityWithName 'topEntity "queue"
