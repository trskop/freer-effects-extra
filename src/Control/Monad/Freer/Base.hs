{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}    -- MonadBase and MonadIO instances.
-- |
-- Module:       $HEADER$
-- Description:  Handle base monad of an effect stack as a special case of an
--               effect.
-- Copyright:    (c) 2016-2017 Peter Trško
-- License:      BSD3
--
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- Handle base monad of an effect stack as a special case of an effect.
module Control.Monad.Freer.Base
    (
    -- * Last Effect and Initial Effects
      LastMember
    , Init

    -- * Base Effect
    , BaseMember
    , sendBase

    -- * MonadBase
    --
    -- | Base monad\/effect of an effect stack may be a monadic stack already.
    -- In such cases one may need to lift a computation through that stack
    -- first and then lift it in to the 'Eff' monad.
    , BaseEff
    , liftBaseEff

    -- * MonadIO
    --
    -- | Base monad\/effect of an effect stack may be a monadic stack built on
    -- top of 'IO'. In such cases one may need to lift a computation through
    -- that stack first and then lift it in to the 'Eff' monad.
    , EffIO
    , liftIOEff
    )
  where

import Control.Monad (Monad)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Bool (Bool(False))
import Data.Function ((.))
import Data.Type.Equality (type (==))
import GHC.TypeLits (TypeError, ErrorMessage((:<>:), ShowType, Text))
import System.IO (IO)

import Control.Monad.Base (MonadBase(liftBase))
import Control.Monad.Freer (Eff, Member, send)


-- {{{ Init -------------------------------------------------------------------

type MissingBaseEffectError (m :: * -> *) =
    'Text "Found '[], but expected '[" ':<>: 'ShowType m ':<>: 'Text "]."

type ExpectedDifferentBaseEffectError (m :: * -> *) (e :: * -> *) =
    'Text "Found '[" ':<>: 'ShowType e ':<>: 'Text "], but expected '["
    ':<>: 'ShowType m ':<>: 'Text "]."

-- | Take all effects from @(effs :: [* -> *])@, except the last effect which
-- must have type @(m :: * -> *)@.
--
-- >>> :t Proxy @(Init IO '[Reader (), State (), IO])
-- Proxy @(Init IO '[Reader (), State (), IO]) :: Proxy '[Reader (), State ()]
type family Init (m :: * -> *) (effs :: [* -> *]) :: [* -> *] where
    Init m       '[]  = TypeError (MissingBaseEffectError m)
    Init m (m ': '[]) = '[]
    Init m (e ': '[]) = TypeError (ExpectedDifferentBaseEffectError m e)
    Init m (e ':  es) = e ': Init m es

-- }}} Init -------------------------------------------------------------------

-- {{{ LastMember -------------------------------------------------------------

-- | Similar as 'Member', but checks if the @(eff :: * -> *)@ is last in the
-- stack of effects. Last effect may have a special meaning, see 'BaseMember'
-- for more details.
class Member eff effs => LastMember (eff :: * -> *) (effs :: [* -> *])
    | effs -> eff

-- | Base case for 'LastMember'.
instance LastMember eff '[eff]

-- | Recursively look for a base effect. We need to make sure that this
-- instance is not overlapping with @instance LastMember m '[m]@, which is the
-- reason for the complex pattern matching on list of effects.
instance
    ( (any1 == eff) ~ 'False
    , Member eff (any1 ': any2 ': effs)
    ) => LastMember eff (any1 ': any2 ': effs)

-- }}} LastMember -------------------------------------------------------------

-- {{{ BaseMember -------------------------------------------------------------

-- | Effects in Haskell are usually built on top of an underlying monad. In
-- such cases the last step of evaluating effect stack is calling 'runM' on an
-- @'Eff' '[m] w@ which gives us @m w@ where @m@ is the underlying monad, e.g.
-- 'IO'.
--
-- In (freer) 'Eff' implementation the base monad is encoded as just a nother
-- effect, but it has specific features, and those aren't captured by
-- considering it to be ordinary effect. This class provides same functionality
-- as 'Member', but it makes a distinction between common effects and effect(s)
-- of underlying monad.
--
-- __Common type errors and their meaning:__
--
-- Couldn't match type @'[]@ with @r'0 : rs'0@:
--
-- @
-- λ> :t 'sendBase' (putStrLn "Hello!") :: 'Eff' '['Control.Monad.Freer.Reader.Reader' (), 'Control.Monad.Freer.State.State' ()] ()
--
-- \<interactive\>:1:1: error:
--     • Couldn't match type ‘'[]’ with ‘r'0 : rs'0’
--         arising from a use of ‘sendBase’
--     • In the expression:
--           'sendBase' (putStrLn "Hello!") :: 'Eff' '['Control.Monad.Freer.Reader.Reader' (), 'Control.Monad.Freer.State.State' ()] ()
-- @
--
-- In the above example 'sendBase' is trying to send a base effect 'IO', but
-- the type signature @'Eff' '['Control.Monad.Freer.Reader.Reader' (),
-- 'Control.Monad.Freer.State' ()] ()@ doesn't even mention it. To correct this
-- issue use @Eff '['Control.Monad.Freer.Reader.Reader' (),
-- 'Control.Monad.Freer.State.State' (), IO] ()@ type signature instead.
--
-- Couldn't match type @'True@ with @'False@:
--
-- @
-- λ> :t 'sendBase' (putStrLn "Hello!") :: 'Eff' '['Control.Monad.Freer.Reader.Reader' (), 'IO', 'Control.Monad.Freer.State.State' ()] ()
--
-- \<interactive\>:1:1: error:
--     • Couldn't match type ‘'True’ with ‘'False’
--         arising from a use of ‘sendBase’
--     • In the expression:
--           'sendBase' (putStrLn "Hello!") :: 'Eff' '['Control.Monad.Freer.Reader.Reader' (), 'IO', 'Control.Monad.Freer.State.State' ()] ()
-- @
--
-- In the above example 'sendBase' is trying to send a base effect 'IO', but
-- the type signature @'Eff' '['Control.Monad.Freer.Reader.Reader' (), 'IO',
-- 'Control.Monad.Freer.State.State' ()] ()@ considers @State ()@ to be a base
-- effect. To correct this issue use @'Eff'
-- '['Control.Monad.Freer.Reader.Reader' (), 'Control.Monad.Freer.State.State'
-- (), 'IO'] ()@ instead.
--
-- There is also another variation of the error message for the above cause:
--
-- @
-- \<interactive\>:1:1: error:
--     • Couldn't match expected type ‘'False’
--                   with actual type ‘Data.Type.Equality.EqArrow r'0 IO’
--       The type variable ‘r'0’ is ambiguous
-- @
class (Monad m, LastMember m effs) => BaseMember m effs | effs -> m

-- | Last effect that is also a monad is considered to be a base monad, i.e.
-- base effect.
instance (Monad m, LastMember m effs) => BaseMember m effs

-- | Function 'send' restricted to sending a base (last) effect.
--
-- >>> :t sendBase (putStrLn "Launch missiles!")
-- sendBase (putStrLn "Launch missiles!") :: BaseMember IO effs => Eff effs ()
sendBase :: BaseMember eff effs => eff a -> Eff effs a
sendBase = send

-- }}} BaseMember -------------------------------------------------------------

-- {{{ MonadBase --------------------------------------------------------------

-- | This instance is used when a base monad\/effect of an effect stack is a
-- monadic stack already, and we need to lift a computation through that stack
-- first and then lift it in to the 'Eff' monad. 'MonadBase' provides
-- functionality for the first part (lifting a base monad in to a monadic
-- stack), and this instance does the rest.
instance (BaseMember m effs, MonadBase b m) => MonadBase b (Eff effs) where
    liftBase = sendBase . (liftBase :: b a -> m a)

-- | Simplified constraint for an effect stack build on top of monadic stack
-- with base monad @b@. Note that base monad @b@ itself can be a trivial
-- monadic stack, i.e. @b = m@. See also 'BaseMember', 'MonadBase', and
-- 'liftBaseEff'.
type BaseEff b m effs = (BaseMember m effs, MonadBase b (Eff effs))

-- | Function 'liftBase' with 'Eff' friendly type signature.
liftBaseEff :: BaseEff b m effs => b a -> Eff effs a
liftBaseEff = liftBase

-- }}} MonadBase --------------------------------------------------------------

-- {{{ MonadIO ----------------------------------------------------------------

-- | Lift 'IO' computation in to 'Eff' monad. This allows computation that
-- operates on arbitrary 'MonadIO' to be a valid effectful computation.
--
-- If we have computation as:
--
-- @
-- launchMissiles :: 'MonadIO' m => m ()
-- launchMissiles = 'liftIO' $ putStrLn "Nuclear launch detected."
-- @
--
-- Then the following will type check:
--
-- @
-- launchMissiles :: 'EffIO' m effs => 'Eff' effs ()
-- @
instance (BaseMember m effs, MonadIO m) => MonadIO (Eff effs) where
    liftIO = sendBase . (liftIO :: IO a -> m a)

-- | Simplified constraint for an effect stack build on top of monadic stack
-- with base monad 'IO'. Note that 'IO' itself is considered as a trivial
-- monadic stack. See also 'BaseMember', 'MonadIO' and 'liftIOEff'.
type EffIO m effs = (BaseMember m effs, MonadIO m)

-- | Function 'liftIO' with 'Eff' friendly type signature. Note that when
-- @m = 'IO'@ then 'liftIOEff' = 'sendBase'.
liftIOEff :: EffIO m effs => IO a -> Eff effs a
liftIOEff = liftIO

-- }}} MonadIO ----------------------------------------------------------------
