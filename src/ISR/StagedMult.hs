{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ISR.StagedMult (Input, Output, ProdBundle) where

import Clash.Prelude

newtype Identity a = Identity { runIdentity :: a }

data ProdBundle t = ProdBundle
  { prod :: t (Unsigned 64),
    mplier :: t (Unsigned 64),
    mcand :: t (Unsigned 64)
  }
deriving instance Generic (ProdBundle t)
-- deriving instance Show (ProdBundle t)
-- deriving instance Eq (ProdBundle t)
-- deriving instance NFDataX (ProdBundle t)
instance Bundle (ProdBundle Identity) where
  type instance Unbundled dom (ProdBundle Identity) = ProdBundle (Signal dom)
  bundle ProdBundle {prod,mplier,mcand} =
    ProdBundle <$> (Identity <$> prod) <*> (Identity <$> mplier)<*> (Identity <$> mcand)
    -- undefined
  unbundle m =
    ProdBundle
      {
        prod = runIdentity . prod <$> m,
        mplier = runIdentity . mplier <$> m,
        mcand = runIdentity . mcand <$> m
      }

data Input t = Input
  { start :: t Bool,
    prods_in :: ProdBundle t
  }
deriving instance Generic (Input t)
-- deriving instance Show (Input t)
-- deriving instance Eq (Input t)
-- deriving instance NFDataX (Input t)
instance Bundle (Input Identity) where
  type instance Unbundled dom (Input Identity) = Input (Signal dom)
  bundle Input {start,prods_in} =
    Input <$> (Identity <$> start) <*> bundle prods_in
    -- undefined
  unbundle m =
    Input
      {
        start = runIdentity . start <$> m,
        prods_in = unbundle $ prods_in <$> m
      }

data Output t = Output
  { prods_out :: ProdBundle t,
    done :: t Bool
  }
deriving instance Generic (Output t)
-- deriving instance Show (Output t)
-- deriving instance Eq (Output t)
-- deriving instance NFDataX (Output t)

instance Bundle (Output Identity) where
  type instance Unbundled dom (Output Identity) = Output (Signal dom)
  bundle Output {prods_out,done} =
    Output <$> bundle prods_out <*> (Identity <$> done)
  unbundle m =
    Output
      {
        prods_out = unbundle $ prods_out <$> m,
        done = runIdentity . done <$> m

      }



-- staged :: (HiddenClockResetEnable dom) => Signal dom (Input Identity) -> Signal dom (Output Identity)
-- staged input =
--   bundle $ Output
--     { prods_out =
--         ProdBundle
--           { prod = partial + prod,
--             mplier = mplier `shiftR` width,
--             mcand = mcand `shiftL` width
--           },
--       done =
--         start
--     }
--   where
--     Input {start, prods_in} = unbundle input
--     start_ = start :: Input (Signal dom)
--     ProdBundle {prod, mplier, mcand} = prods_in
--     width = 8
--     partial = ((unpack $ zeroBits ++# slice d7 d0 mplier) :: Unsigned 64) * mcand
