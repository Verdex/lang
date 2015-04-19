

module MState where

import Control.Applicative


data MSResult s a = Success s a
                | Failure 
data MState s a = MState ( s -> MSResult s a )
proj (MState f) = f
    
instance Functor (MState s) where
    fmap f a = MState $ \ s -> handleResult $ proj a s 

        where handleResult (Success s a) = Success s (f a)
              handleResult Failure = Failure

instance Applicative (MState s) where
    pure a = MState $ \ s -> Success s a
    ms1 <*> ms2 = 
        do
            f <- ms1
            a <- ms2
            return $ f a

instance Monad (MState s) where
    return = pure
    ms >>= gen = mu $ fmap gen ms

        where mu ms = MState $ \ s -> inner $ proj ms s 
              inner Failure = Failure
              inner (Success s a) = proj a s 

failure :: MState s a
failure = MState $ \ s -> Failure

success :: MState s ()
success = pure ()

finalMState :: MState s a -> s -> Maybe s
finalMState ms init = hr $ proj ms init

    where hr (Success s a) = Just s
          hr Failure = Nothing

getState :: MState s s
getState = MState $ \ s -> Success s s

setState :: s -> MState s ()
setState s = MState $ \ s' -> Success s () 


