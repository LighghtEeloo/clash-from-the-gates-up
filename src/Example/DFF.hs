module Example.DFF (dff) where

import Clash.Prelude

-- | Double Flip Flop
dff ::
  (HiddenClockResetEnable dom, NFDataX a) =>
  a -> Signal dom Bool ->
  Signal dom a -> Signal dom a -> Signal dom a
dff d drst dd a = register d a'
  where
    a' = mux drst dd a
    -- a' = (\drst_val dd a -> if drst_val then dd else a) <$> (drst <*> dd <*> a)
  -- (drst ? dd : a)
