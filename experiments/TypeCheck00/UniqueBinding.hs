
module UniqueBinding where

import State

type UB k v = (Integer, [(k, v)])

nullBinding :: (Integer, [(k, v)])
nullBinding = (0, [])

newInt :: State (UB k v) Integer
newInt = 
    do
        (s, ctx) <- getState
        setState (s + 1, ctx)
        return s

setLink :: k -> v -> State (UB k v) ()
setLink k v = 
    do
        (s, ctx) <- getState
        setState $ (s, (k, v) : ctx)

lookupLink :: Eq k => k -> State (UB k v) (Maybe v)
lookupLink k = 
    do
        (_, ctx) <- getState
        return $ lookup k ctx
