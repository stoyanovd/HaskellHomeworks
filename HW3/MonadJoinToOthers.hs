{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import MonadsHeaders
import Prelude hiding (Monad(..))

instance Monad m => MonadFish m where
    returnFish = return
    f >=> g = \x -> (((returnFish x) >>= f) >>= g)

instance Monad m => MonadJoin m where
    returnJoin = return
    join = \x -> (x >>= \y -> y)