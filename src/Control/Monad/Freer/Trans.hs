--{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
--{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--{-# OPTIONS_GHC -Wno-orphans #-}    -- MonadBase and MonadIO instances.
-- |
-- Module:       $HEADER$
-- Description:  Include effects as a monad transformer.
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Control.Monad.Freer.Trans
    ( Append
    , EffT(..)
    , SendBase
    , sendBaseT
    )
  where

import Control.Applicative (Applicative)
import Control.Monad (Monad, (>>=))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Either (Either(Left, Right))
import Data.Function ((.))
import Data.Functor (Functor)
import Data.Proxy (Proxy(Proxy))
--import Data.Type.Equality (type (==))
--import GHC.TypeLits (TypeError, ErrorMessage((:<>:), ShowType, Text))

import Control.Monad.Morph (MFunctor(hoist))
import Control.Monad.Trans.Class (MonadTrans(lift))

import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Freer.Internal (Eff(E, Val), Member, decomp, qComp, send, tsingleton)

import Control.Monad.Freer.Class (weakenEff)


type family Append (m :: * -> *) (effs :: [* -> *]) :: [* -> *] where
    Append m '[]       = '[m]
    Append m (e ': es) = e ': Append m es

newtype EffT effs m a = EffT {runEffT :: Eff (Append m effs) a}
  deriving (Applicative, Functor, Monad)

class SendBase effs where
    sendBase' :: Append m effs ~ effs' => proxy effs -> m a -> Eff effs' a

sendBaseT :: forall effs m a. SendBase effs => m a -> EffT effs m a
sendBaseT = EffT . sendBase' (Proxy :: Proxy effs)
{-# INLINE sendBaseT #-}

instance SendBase '[] where
    sendBase' _ = send -- :: m a -> Eff '[m] a
    {-# INLINE sendBase' #-}

instance SendBase es => SendBase (e ': es) where
    sendBase' _ = weakenEff . sendBase' (Proxy :: Proxy es)
    {-# INLINE sendBase' #-}

instance SendBase effs => MonadTrans (EffT effs) where
    lift = sendBaseT
    {-# INLINE lift #-}

instance (SendBase effs, MonadIO m) => MonadIO (EffT effs m) where
    liftIO = sendBaseT . liftIO
    {-# INLINE liftIO #-}

instance (SendBase effs, MonadBase b m) => MonadBase b (EffT effs m) where
    liftBase = sendBaseT . (liftBase :: b a -> m a)
    {-# INLINE liftBase #-}

instance SendBase effs => MFunctor (EffT effs) where
    -- :: Monad m => (forall a. m a -> n a) -> EffT effs m b -> EffT effs n b
    hoist = hoist

    {-
    hoist f (EffT m) = EffT $ loop m
      where
        loop (Val x)  = ret x
        loop (E u' q) = case decomp u' of
            Right x -> send (f x) >>= k
            Left  u -> E u (tsingleton k)
          where
            k = q `qComp` loop
    -}

--hoistEff ::

{- Will this work?
instance MMonad (EffT effs)
-}
