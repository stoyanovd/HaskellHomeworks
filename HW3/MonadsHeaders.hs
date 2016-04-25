{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, UndecidableInstances #-}
module MonadsHeaders(Monad(..), MonadFish(..), MonadJoin(..)) where

import Prelude hiding (Monad(..))

class Monad m where
    return :: a -> m a
    (>>=) :: (m a) -> (a -> m b) -> (m b)

class MonadFish m where
    returnFish :: a -> m a
    (>=>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

class MonadJoin m where
    returnJoin :: a -> m a
    join :: m (m a) -> m a

instance Monad Maybe where
    return x = Just x
    Nothing >>= f = Nothing
    Just x >>= f = f x
