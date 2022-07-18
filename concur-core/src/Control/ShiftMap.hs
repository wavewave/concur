{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}
module Control.ShiftMap where

import           Control.Natural              (type (~>))

import           Control.Monad.State          (StateT, mapStateT)
import           Control.Monad.Trans.Identity (IdentityT, mapIdentityT)

-- | Mapping between Natural Transformations
class ShiftMap s t where
  shiftMap :: (s ~> s) -> (t ~> t)

instance ShiftMap m m where
  shiftMap f = f

instance ShiftMap m (IdentityT m) where
  shiftMap f = mapIdentityT f

instance ShiftMap m (StateT s m) where
  shiftMap f = mapStateT f
