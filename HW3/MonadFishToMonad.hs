{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
import MonadsHeaders
import Prelude hiding (Monad(..))

instance MonadFish m => Monad m where
    return = returnFish
    x >>= f = (id >=> f) x