{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
-- |
-- Module:       $HEADER$
-- Description:  TODO
-- Copyright:    (c) 2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- TODO
module Control.Monad.Freer.Control
    (
    -- * Unlifting Effects
    --
    -- | Inspired by following packages:
    --
    -- * <https://hackage.haskell.org/package/monad-control monad-control>
    -- * <https://hackage.haskell.org/package/monad-unlift monad-unlift>
      Run(..)
    , EffectControl(..)
    , toBase
    , toBaseWith
    , fromBase

    -- ** Unlifting Individual Effects
    , RunEffect(..)
    , ControlableEffect(..)
    , EffectState(..)
    , runEffect
    )
  where

import Control.Applicative (Applicative, pure)
import Control.Monad (Monad, (>>=))
import Data.Function (($), (.), flip)
import Data.Functor (Functor(fmap), (<$), (<$>))
import Data.Functor.Compose (Compose(Compose), getCompose)
import Data.Functor.Identity (Identity(Identity), runIdentity)

import Control.Monad.Freer (Eff, handleRelayS, runM, send)
--import Control.Monad.Freer.Exception (Exc)
import Control.Monad.Freer.Reader (Reader(Reader), ask)
import Control.Monad.Freer.State (State(Get, Put), get, put)

import Control.Monad.Freer.Class (weakenEff)
import Control.Monad.Freer.Base (BaseMember, Init, sendBase)



-- {{{ EffectControl ----------------------------------------------------------

-- | Represents a stack of functions which can move an action down the effect
-- stack, by providing any necessary environment for the effects.
data Run (m :: * -> *) (effs :: [* -> *]) where
    RunHeadEffect :: RunEffect m e -> Run m effs -> Run m (e ': effs)
    RunDone :: Run m '[]

class
    ( BaseMember m effs
    ) => EffectControl (m :: * -> *) (effs :: [* -> *])
    | effs -> m
  where
    type StE effs (f :: * -> *) (r :: *)

    -- | Change representation of effect stack so that the base effect is
    -- directly exposed.
    runEffects
        :: Functor f
        => Eff effs (f a)
        -> Run m (Init m effs)
        -> Eff '[m] (StE effs f a)

    -- | Dual to 'runEffects' that restores the effect stack.
    restoreEffects
        :: Functor f
        => (Run m (Init m effs) -> m (StE effs f a))
        -> Eff effs (f a)

-- | Base case.
instance Monad m => EffectControl m '[m] where
    type StE '[m] f r = f r

    runEffects m RunDone = m
    restoreEffects f = send $ f RunDone

-- | Recursion.
instance forall m eff eff' effs .
    ( BaseMember m (eff ': eff' ': effs) , EffectControl m (eff' ': effs)
    , ControlableEffect eff
    ) => EffectControl m (eff ': (eff' ': effs))
  where
    type StE (eff ': eff' ': effs) f r =
        StE (eff' ': effs) (Compose f (EffectState eff)) r
        -- Following is an example of running multiple effects:
        --
        --   \m e1 e2 e3 ->
        --       runEffectWith e1
        --           $ runEffectWith e2
        --               $ runEffectWith e3
        --                   $ Identity <$> m
        --   :: Something -- Constraints are currently irrelevant.
        --   => Eff (e2 : e1 : e : t : ts) a
        --   -> RunEffect m e
        --   -> RunEffect m e1
        --   -> RunEffect m e2
        --   -> Eff (t : ts)
        --       ( Compose
        --           ( Compose
        --               (Compose Identity (EffectState e2))
        --               (EffectState e1)
        --           )
        --           (EffectState e)
        --           a
        --       )
        --
        -- Let "type (∘) = Compose" then the functor in result value can be
        -- rewritten as:
        --
        --   ((Identity ∘ EffectState e2) ∘ EffectState e1) ∘ EffectState e
        --
        -- Now, if we consider there to be a generic functor instead of
        -- Identity:
        --
        --   ((f ∘ EffectState e2) ∘ EffectState e1) ∘ EffectState e
        --
        -- StE has to encode a scheme for constructing the above:
        --
        --   StE (e2 : e1 : e : t : ts) f r
        --       = StE (e1 : e : t : ts) (f ∘ EffectState e2) r
        --       = StE (e : t : ts) ((f ∘ EffectState e2) ∘ EffectState e1) r
        --       = ...

    -- :: Functor f
    -- => Eff (eff ': eff' ': effs) (f a)
    -- -> Run m (Init m (eff ': eff' ': effs))
    -- -> Eff '[m] (StE (eff ': eff' ': effs) f a)
    runEffects m (RunHeadEffect run runs) =
        runEffects (runEffectWith run m) runs
        -- runEffectWith
        --     ::  ( Functor f
        --         , ControlableEffect eff
        --         , BaseMember m (eff' ': effs)
        --         )
        --     => RunEffect m e
        --     -> Eff (eff ': eff' ': effs) (f a)
        --     -> Eff (eff' ': effs) (Compose f (EffectState e) a)

    -- :: Functor f
    -- =>  ( Run m (Init m (eff ': eff' ': effs))
    --         -> m (StE (eff ': eff' ': effs) f a)
    --     )
    -- -> Eff (eff ': eff' ': effs) (f a)
    restoreEffects = restoreEffects -- FIXME!

    {-
    restoreEffects f = restoreEffect $ \run ->
        fmap getCompose $ restoreEffects $ \runs ->
            f (RunHeadEffect run runs :: Run m (eff : eff' : effs))
    -}

    {-
    restoreEffects = restoreEffects'
      where
        restoreEffects'
            ::  ( Run m (Init m (eff : eff' : effs))
                    -> m (StE (eff' : effs) (Compose (EffectState eff) f) a)
                )
            -> Eff (eff : eff' : effs) (f a)
        restoreEffects' f = restoreEffect $ \run ->
            fmap getCompose $ restoreEffects $ \runs ->
                f (RunHeadEffect run runs :: Run m (eff : eff' : effs))
    -}

    -- THE PROBLEM: Association of "EffectState eff" functors is wrong.

-- | Change representation of effect stack so that the base effect is
-- directly exposed.
--
-- When made explicit, the default association of (->), then we get:
--
-- @
-- 'toBase'
--     :: 'EffectControl' m effs
--     => 'Eff' effs a
--     -> ('Run' ('Init' m effs) -> m ('StE' effs 'Identity' a))
-- @
--
-- The above type signature makes it easier to spot what 'toBase'
-- actually does, and what is its relation to 'fromBase'.
toBase
    :: EffectControl m effs
    => Eff effs a
    -> Run m (Init m effs)
    -> m (StE effs Identity a)
toBase m es = runM $ runEffects (Identity <$> m) es

-- | Variant of 'toBase' with its arguments flipped.
toBaseWith
    :: EffectControl m effs
    => Run m (Init m effs) -> Eff effs a
    -> m (StE effs Identity a)
toBaseWith = flip toBase

-- | Dual to 'toBase' that restores the effect stack.
fromBase
    :: EffectControl m effs
    => (Run m (Init m effs) -> m (StE effs Identity a))
    -> Eff effs a
fromBase f = runIdentity <$> restoreEffects f

-- }}} EffectControl ----------------------------------------------------------

-- {{{ ControlableEffect ------------------------------------------------------

-- | Existential cloasure, for a function, which interprets the effect
-- @(e :: * -> *)@ in terms of base effect @(m :: * -> *)@.
data RunEffect (m :: * -> *) (e :: * -> *) where
    RunEffect
        :: EffectState e ()
        -> (forall r. EffectState e () -> e r -> m (EffectState e r))
        -> RunEffect m e

-- | Class of effects that can be evaluated in terms of base effect (see also
-- 'BaseMember').
--
-- Instances need to flollow this law:
--
-- @
-- \\m -> 'restoreEffect' $ \\e ->
--     'runIdentity' . 'getCompose' <$> 'runEffectWith' e ('Identity' <$> m)
-- @
class Functor (EffectState e) => ControlableEffect (e :: * -> *) where
    {-# MINIMAL runEffectWith, restoreEffect #-}

    -- | State of an effect that needs to be passed around.
    --
    -- For e.g. @(State s :: * -> *)@ effect this would encode passing the
    -- state @s :: *@ around to preserve updates.
    data EffectState e r

    -- | Evaluate top effect in terms of base effect (@'BaseMember' m@) and
    -- remove it from the effect stack.
    runEffectWith
        :: (Functor f, BaseMember m effs)
        => RunEffect m e
        -> Eff (e ': effs) (f a)
        -> Eff effs (Compose f (EffectState e) a)
        -- ^ 'Compose' allows stacking effect states on top of each other in
        -- the correct order when calling 'runEffectWith' repeatedly.

    -- | Restore effect on top of the effect stack.
    restoreEffect
        :: BaseMember m effs
        => (RunEffect m e -> Eff effs (EffectState e a))
        -> Eff (e ': effs) a

instance forall e. ControlableEffect (Reader e) where
    newtype EffectState (Reader e) r = StReader (r, e)
        -- This doesn't look totally right, since it's the same as for State.
        -- See also runEffectWith, which has similar symptoms. However,
        -- expected definition "... = StReader r" doesn't work, sinece we need
        -- to remember the initial value of the environment.

    runEffectWith (RunEffect (StReader (_, e)) f) =
        handleRelayS e pure' $ \e' eff@Reader k ->
            sendBase (f (StReader ((), e')) eff)
                >>= \(StReader (r, e'')) -> k e'' r
      where
        pure' env fa = pure . Compose $ (\a -> StReader (a, env)) <$> fa

    restoreEffect f = do
        st <- StReader . ((),) <$> ask
        StReader (r, _) <- weakenEff $ f (RunEffect st evalReader)
        pure r
      where
        evalReader
            :: Applicative m
            => EffectState (Reader e) ()
            -> Reader e r
            -> m (EffectState (Reader e) r)
        evalReader (StReader (_, e)) Reader = pure (StReader (e, e))

instance Functor (EffectState (Reader e)) where
    fmap f (StReader (r, e)) = StReader (f r, e)

instance ControlableEffect (State s) where
    newtype EffectState (State s) r = StState (r, s)

    runEffectWith (RunEffect (StState (_, s)) f) =
        handleRelayS s pure' $ \s' eff k ->
            sendBase (f (StState ((), s')) eff)
                >>= \(StState (r, s'')) -> k s'' r
      where
        pure' state fa = pure . Compose $ (\a -> StState (a, state)) <$> fa

    restoreEffect f = do
        st <- StState . ((),) <$> get
        StState (r, s') <- weakenEff $ f (RunEffect st evalState)
        r <$ put s'
      where
        evalState
            :: Applicative m
            => EffectState (State s) ()
            -> State s r
            -> m (EffectState (State s) r)
        evalState (StState (_, s)) = \case
            Get -> pure (StState (s, s))
            Put s' -> pure (StState ((), s'))

instance Functor (EffectState (State s)) where
    fmap f (StState (r, s)) = StState (f r, s)

-- | Variant of 'runEffectWith' with its arguments flipped'
runEffect
    :: (Functor f, ControlableEffect e, BaseMember m effs)
    => Eff (e ': effs) (f a)
    -> RunEffect m e
    -> Eff effs (Compose f (EffectState e) a)
runEffect = flip runEffectWith

-- }}} ControlableEffect ------------------------------------------------------
