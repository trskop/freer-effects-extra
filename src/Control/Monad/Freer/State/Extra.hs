{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
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
    (
    -- * Re-export Module That's Beeing Extended
    --
    -- | Everything is beeing re-exported from it.
      module Control.Monad.Freer.State

    -- * Effect Evaluation
    , runStateM
    , runStateAsBase

    -- * Effect Operations
    , gets
    , modify'
    , state

    -- ** Support For Lenses
    --
    -- | Be aware that these definitions clash with exports from
    -- "Control.Lens". Reason for this decision is the fact that /lens/
    -- definitions use @MonadState@ constraint, which doesn't have such special
    -- case as @MonadReader r ((->) r)@, therefore, these definitions aren't
    -- required to be applicable outside of 'State' effect context.

    -- *** Lens Getters
    , use
    , uses
    , iuse
    , iuses

    -- *** Lens Setters
    , assign
    , modifying
    , (%=)
    , (%%=)
    , (?=)
    )
  where

import Prelude (($!))

import Control.Applicative (pure)
import Control.Monad ((>>), (>>=))
import Data.Either (Either(Left, Right))
import Data.Function (($), (.))
import Data.Functor ((<$>))
import Data.Functor.Const (Const(Const, getConst))  -- base >=4.9
import Data.Maybe (Maybe)
import Unsafe.Coerce (unsafeCoerce)

import Control.Lens
    ( ASetter
    , Getting
    , Indexed(Indexed)
    , IndexedGetting
    , LensLike'
    , Over
    , (%~)
    , (.~)
    , (?~)
    , view
    , views
    )
import Control.Monad.Freer (Eff, Member, send)
import qualified Control.Monad.Freer.Internal as Internal
    ( Eff(E, Val)
    , decomp
    , qApp
    , tsingleton
    )
import Control.Monad.Freer.State
import Control.Monad.State.Class (MonadState)
import qualified Control.Monad.State.Class as MonadState (get, put)
import Data.Profunctor.Unsafe as Unsafe ((#.))

import Control.Monad.Freer.Base (BaseMember)


-- {{{ Effect Evaluation ------------------------------------------------------

-- | This contraption is here only because 'State' doesn't expose its data
-- constructors.
data State' s v where
    Get :: State' s s
    Put :: !s -> State' s ()
    -- TODO: Send pull-request to freer which exposes State's data
    --       constructors.

-- | Evaluate 'State' effect in terms of base effect using specified base
-- effect operations.
runStateAsBase
    :: forall s m effs a
    .  BaseMember m effs
    => m s
    -> (s -> m ())
    -> Eff (State s ': effs) a
    -> Eff effs a
runStateAsBase baseGet basePut = \case
    Internal.Val x -> pure x
    Internal.E u q -> case Internal.decomp u of
        Left  u' -> Internal.E u' . Internal.tsingleton
            $ runStateAsBase' . Internal.qApp q
        Right u' -> case coerce u' of
            Get   -> send' baseGet >>= runStateAsBase' . Internal.qApp q
            Put s ->
                send' (basePut s) >> runStateAsBase' (q `Internal.qApp` ())
  where
    runStateAsBase' :: Eff (State s ': effs) a -> Eff effs a
    runStateAsBase' = runStateAsBase baseGet basePut

    send' :: m b -> Eff effs b
    send' = send

    -- TODO: Send pull-request to freer which exposes State's data
    --       constructors.
    coerce :: State s b -> State' s b
    coerce = unsafeCoerce
{-# INLINEABLE runStateAsBase #-}

-- | Evaluate 'State' effect in terms of base effect, a monad that has
-- 'MonadState' capabilities.
--
-- This function is just a specialisation of 'runStateAsBase':
--
-- @
-- 'runStateM' = 'runStateAsBase' 'MonadState.get' 'MonadState.put'
-- @
runStateM
    :: forall s m effs a
    .  (MonadState s m, BaseMember m effs)
    => Eff (State s ': effs) a
    -> Eff effs a
runStateM = runStateAsBase MonadState.get MonadState.put
{-# INLINE runStateM #-}

-- }}} Effect Evaluation ------------------------------------------------------

-- {{{ Effect Operations ------------------------------------------------------

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

-- {{{ Effect Operations -- Lens ----------------------------------------------

-- | Variant of Control.Lens.use' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
use :: Member (State s) effs => Getting a s a -> Eff effs a
use = gets . view
{-# INLINE use #-}

-- | Variant of 'uses' that works for 'State' effect.
uses
    :: Member (State s) effs
    => LensLike' (Const r) s a
    -> (a -> r)
    -> Eff effs r
uses l = gets . views l
{-# INLINE uses #-}

-- | Variant of 'Control.Lens.iuse' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
iuse
    :: Member (State s) effs
    => IndexedGetting i (i, a) s a
    -> Eff effs (i, a)
iuse l = gets (getConst Unsafe.#. l (Indexed $ \i -> Const Unsafe.#. (,) i))
{-# INLINE iuse #-}

-- | Variant of 'Control.Lens.iuses' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
iuses
    :: Member (State s) effs
    => IndexedGetting i r s a
    -> (i -> a -> r)
    -> Eff effs r
iuses l f = gets (getConst Unsafe.#. l (Const Unsafe.#. Indexed f))
{-# INLINE iuses #-}

-- | Variant of 'Control.Lens.assign' from "Control.Lens" that works for
-- 'State' effect. See /lens/ documentation for more details, and examples.
assign :: Member (State s) effs => ASetter s s a b -> b -> Eff effs ()
assign l b = modify (l .~ b)
{-# INLINE assign #-}

-- | Variant of 'Control.Lens.%=' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
(%=) :: Member (State s) effs => ASetter s s a b -> (a -> b) -> Eff effs ()
l %= f = modify (l %~ f)
{-# INLINE (%=) #-}

-- | This is an alias for ('%='), see its documentation for more details.
modifying
    :: Member (State s) effs
    => ASetter s s a b
    -> (a -> b)
    -> Eff effs ()
modifying = (%=)
{-# INLINE modifying #-}

-- | Variant of 'Control.Lens.%%=' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
(%%=)
    :: Member (State s) effs
    => Over p ((,) r) s s a b
    -> p a (r, b)
    -> Eff effs r
l %%= f = state (l f)
{-# INLINE (%%=) #-}

-- | Variant of 'Control.Lens.?=' from "Control.Lens" that works for 'State'
-- effect. See /lens/ documentation for more details, and examples.
(?=) :: Member (State s) effs => ASetter s s a (Maybe b) -> b -> Eff effs ()
l ?= b = modify (l ?~ b)
{-# INLINE (?=) #-}

-- }}} Effect Operations -- Lens ----------------------------------------------

-- }}} Effect Operations ------------------------------------------------------
