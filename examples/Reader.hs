{-# LANGUAGE GADTs, TypeSynonymInstances,LambdaCase #-}

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans.Operational.Mini

data ReaderOpr e m a where
  Ask :: ReaderOpr e m e
  Local :: (e -> e) -> ReaderT e m a -> ReaderOpr e m a

type ReaderT e m =  ProgramT (ReaderOpr e m) m
type Reader e = ReaderT e Identity

ask :: Monad m => ReaderT e m e
ask = singleton Ask

local :: Monad m => (e -> e) -> ReaderT e m b -> ReaderT e m b
local f = singleton . Local f

{-|

>>> runReaderT ask 3
3
   
>>> runReaderT (local (+1) ask) 3
4

>>> runReaderT (local (+1) (ask >>= lift . print)) 3
4

-}
runReaderT :: Monad m => ReaderT e m a -> e -> m a
runReaderT c e = interpret (\case
                               Ask -> return e
                               Local f c -> runReaderT c (f e)) c
--runReaderT c e = interpret go c where
--  go Ask = return e
--  go (Local f c) = runReaderT c (f e)


runReader :: Reader e a -> e -> a
runReader c = runIdentity . runReaderT c
