{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
module Ourstuff.ExistentialsExperimentials where

import GHC.TypeLits (Symbol)

data Foo where
    Foo :: Typeable v => v -> Foo

runFoo :: (forall a. Typeable a => a -> result) -> Foo -> result
runFoo fun (Foo v) = fun v


------

data L where
    L :: forall (name :: Symbol) a. Proxy name -> Int -> L

-- runL :: forall (n :: Symbol). (Int -> Text) -> L -> Maybe Text
-- runL fun l =
--     case l of
--       L (Proxy :: Proxy n) value -> Just fun value

------

data IxList [(Symbol, t)] where

