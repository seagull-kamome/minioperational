{-# LANGUAGE GADTs, TypeSynonymInstances,LambdaCase,KindSignatures,ScopedTypeVariables #-}

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Operational.Mini

data StateOpr e a where
  Get :: StateOpr e e
  Put :: e -> StateOpr e ()

type StateT e =  ReifiedProgramT (StateOpr e)
type State e = StateT e Identity

get :: Monad m => StateT e m e
get = singleton Get

put  :: Monad m => e -> StateT e m ()
put = singleton . Put

{-|

>>> runStateT (return 1) 2
(1,1)

>>> runStateT (get >>= lift . print >> put 2 >> get >>= return) 3
3
(1,2)

-}
runStateT :: Monad m => StateT e m a -> e -> m (a,e)
runStateT (Return x) e = return (x,e)
runStateT (Lift x f) e = x >>= \x' -> runStateT (f x') e
runStateT ((:>>=) (Put e') c) _ = runStateT (c ()) e'
runStateT ((:>>=) Get c) e = runStateT (c e) e

runState :: State e a -> e -> (a,e)
runState c = runIdentity . runStateT c
