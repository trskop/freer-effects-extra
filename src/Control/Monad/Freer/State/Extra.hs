{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- |
-- Module:       $HEADER$
-- Description:  Type class MonadEff.
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- <https://jaspervdj.be/posts/2015-01-20-haskell-design-patterns-extended-modules.html Extended module>
-- built on top of "Control.Monad.Freer.State".
module Control.Monad.Freer.State.Extra
    ( module Control.Monad.Freer.State
    , gets
    , modify'
    , state
    )
  where

import Prelude (($!))

import Control.Applicative (pure)
import Data.Functor ((<$>))

import Control.Monad.Freer (Eff, Member)
import Control.Monad.Freer.State


-- | Get a specific component of the state.
gets :: Member (State s) effs => (s -> a) -> Eff effs a
gets f = f <$> get
{-# INLINE gets #-}

-- | A variant of 'modify' in which the computation is strict in the new state.
modify' :: Member (State s) effs => (s -> s) -> Eff effs ()
modify' f = do
    s <- get
    put $! f s
{-# INLINE modify' #-}

-- | Update and query state at the same time.
state :: Member (State s) effs => (s -> (a, s)) -> Eff effs a
state f = do
    (a, s) <- gets f
    put s
    pure a
{-# INLINE state #-}
