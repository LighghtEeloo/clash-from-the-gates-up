-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Q.Project where

-- import Clash.Annotations.TH (makeTopEntityWithName)
import Clash.Prelude hiding (init)
import Control.Lens hiding (Index, indices)
import Q.Circular
import Utils (monomorphizeEntity)

data Input pro con a = Input
  { _producerIn :: "producer_in" ::: Arity pro,
    _consumerIn :: "consumer_in" ::: View con a
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Input

data Output pro con a = Output
  { _producerOut :: "producer_out" ::: View pro a,
    _consumerOut :: "consumer_out" ::: Arity con
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (NFDataX)

makeLenses ''Output

trans :: (KnownNat pro, KnownNat con, KnownNat n, NFDataX a) => State n a -> Input pro con a -> State n a
trans s _i = s

out :: (KnownNat pro, KnownNat con, KnownNat n, NFDataX a) => State n a -> Output pro con a
out _s = undefined

entity ::
  (HiddenClockResetEnable dom, KnownNat pro, KnownNat con, KnownNat n, NFDataX a) =>
  State n a ->
  Signal dom (Input pro con a) ->
  Signal dom (Output pro con a)
entity = moore trans out

type Width = 2

type Data = BitVector 64

topEntity ::
  "clock" ::: Clock System ->
  "reset" ::: Reset System ->
  "enable" ::: Enable System ->
  Signal System (Input Width Width Data) ->
  Signal System (Output Width Width Data)
topEntity =
  monomorphizeEntity $ entity $ State xs 0 0
  where
    xs = map (const $ unpack 0) (indices d8)

-- makeTopEntityWithName 'topEntity "queue"
{-# ANN
  topEntity
  Synthesize
    { t_name = "queue",
      t_inputs =
        [ PortName "clock",
          PortName "reset",
          PortName "enable",
          PortProduct "" [PortName "producer_in", PortName "consumer_in"]
        ],
      t_output =
        PortProduct
          ""
          [PortName "producer_out", PortName "consumer_out"]
    }
  #-}
