
module MMT where

import Control.Applicative
import Control.Monad.Trans.Class

-- This should just be a reimplementation of Maybe Monad Transformer

data MMT m a = MMT (m (Maybe a))

proj :: MMT m a -> m (Maybe a)
proj (MMT m) = m


instance Monad m => Functor (MMT m) where 
    fmap f (MMT m) = MMT $ m >>= (blarg f)

        where blarg _ Nothing = return Nothing
              blarg f (Just a) = return( Just $ f a )

instance Monad m => Applicative (MMT m) where
    pure = MMT . return . Just
    f <*> a = 
        do
            f' <- f
            a' <- a
            return $ f' a'
 
instance Monad m => Monad (MMT m) where
    return = pure 
    i >>= g = MMT $ do
                        mv <- proj i
                        case mv of
                            Nothing -> return Nothing
                            (Just v) -> proj $ g v


instance MonadTrans MMT where
    lift m = MMT $ m >>= return . Just
