{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import MonadsHeaders
import Prelude hiding (Monad(..))

instance MonadFish m => MonadJoin m where
    returnJoin = returnFish
    join = id >=> id

instance Monad m => MonadFish m where
    returnFish = return
    (f >=> g) x = (return x >>= f) >>= g