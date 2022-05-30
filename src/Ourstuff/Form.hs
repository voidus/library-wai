{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Ourstuff.Form where

import Prelude hiding (fmap)

import Data.List qualified as List
import Data.Text qualified as T
import Data.Type.Bool (If, type (||))
import GHC.TypeLits (ErrorMessage (ShowType, Text, (:<>:)), KnownSymbol, Symbol, TypeError, symbolVal)
import Text.Show qualified


--------------
-- Hmap

data HMap (f :: Type -> Type) (fields :: Fields) where
    HMapNil :: HMap f '[]
    HMapCons ::
        forall (name :: Symbol) (t :: Type) (fs :: Fields) (f :: Type -> Type).
        f t ->
        HMap f fs ->
        HMap f ('(name, t) ': fs)


instance Text.Show.Show (HMap f '[]) where
    show HMapNil = ""


instance (KnownSymbol name, Show (f t), Show (HMap f rest)) => Show (HMap f ('(name, t) ': rest)) where
    show :: HMap f ('(name, t) ': rest) -> String
    show (HMapCons v _) = "(" <> symbolVal @name Proxy <> ":" <> show v <> ")"


class HMapGet name t map | name map -> t where
    hMapGet :: map -> t


instance {-# OVERLAPPING #-} HMapGet name (f t) (HMap f ('(name, t) ': rest)) where
    hMapGet (HMapCons v _) = v
instance HMapGet name (f t) (HMap f rest) => HMapGet name (f t) (HMap f (nonMatching ': rest)) where
    hMapGet (HMapCons _ rest) = hMapGet @name rest


type family Union (f1 :: Fields) (f2 :: Fields) where
    Union '[] f = f
    Union f '[] = f
    Union (f ': fs) gs = f ': Union fs gs

hMapMerge :: HMap f fs -> HMap f gs -> HMap f (Union fs gs)
hMapMerge HMapNil rest = rest
hMapMerge rest HMapNil = rest
hMapMerge (HMapCons v rest) other@(HMapCons _ _) = HMapCons v $ hMapMerge rest other

-----------------
-- View

type Fields = [(Symbol, Type)]


data ViewEntry t
    = Missing
    | ParseFailed Text
    | Parsed Text t
    deriving stock (Show, Eq)


newtype View (fields :: Fields) = View (HMap ViewEntry fields)


instance Show (HMap ViewEntry fields) => Show (View fields) where
    show (View m) = "View (" <> show m <> ")"


class GetView name t fields | name fields -> t where
    getFromView :: View fields -> ViewEntry t
instance HMapGet name (ViewEntry t) (HMap ViewEntry fields) => GetView name t fields where
    getFromView (View m) = hMapGet @name m


type family TheType (name :: Symbol) (fields :: Fields) :: Type where
    TheType name '[] = TypeError ( 'Text "No entry found for " ':<>: 'ShowType name)
    TheType name ('(name, t) ': _) = t
    TheType name (_ ': rest) = TheType name rest


type Params = [(Text, Text)]


newtype (name :: Symbol) :> a = (:>) a


type family MappingValue mapping where
    MappingValue (_name :> a) = a


mergeViews :: View f1 -> View f2 -> View (Union f1 f2)
mergeViews (View m1) (View m2) = View $ hMapMerge m1 m2


newtype Form (fields :: Fields) a = Form {runForm :: Params -> (View fields, Maybe a)}


-- "Functor"
fmap, (<$>) :: (a -> b) -> Form fields a -> Form fields b
fmap fun (Form formFun) = Form $ \p -> let (v, mA) = formFun p in (v, fun Prelude.<$> mA)
(<$>) = fmap


-- "Applicative"
pure :: a -> Form '[] a
pure = Form . const . (View HMapNil,) . Just


-- type family Union (f1 :: [k]) (f2 :: [k]) where
--     Union '[] f2 = f2
--     Union f1 '[] = f1
--     Union (f ': f1) f2 = If (f `In` f2) (TypeError ( 'Text "Oopsie")) (f ': Union f1 f2)


type family In (a :: k) (as :: [k]) :: Bool where
    In _ '[] = 'False
    In a (a ': _) = 'True
    In a (_ ': as) = In a as


type family Overlapping (f1 :: [k]) (f2 :: [k]) :: Bool where
    Overlapping '[] _ = 'False
    Overlapping _ '[] = 'False
    Overlapping (f ': f1) f2 = f `In` f2 || Overlapping f1 f2


type family OnlyNames (fields :: [(Symbol, Type)]) :: [Symbol] where
    OnlyNames '[] = '[]
    OnlyNames ('(name, _) ': xs) = name ': OnlyNames xs


(<*>) ::
    forall (fields1 :: Fields) (fields2 :: Fields) a b.
    Form fields1 (a -> b) ->
    Form fields2 a ->
    Form (Union fields1 fields2) b
Form f1 <*> Form f2 = Form \params ->
    let (view1, result1) = f1 params
        (view2, result2) = f2 params
     in (view1 `mergeViews` view2, result1 Prelude.<*> result2)


number :: forall name. KnownSymbol name => Form '[ '(name, Integer)] Integer
number = Form $ \params ->
    case List.lookup (T.pack $ symbolVal @name Proxy) params of
      Nothing -> (View (HMapCons Missing HMapNil), Nothing)
      Just value ->
          let (viewEntry, result) =
                  case readMaybe (T.unpack value) of
                    Just parsed -> (Parsed value parsed, Just parsed)
                    Nothing -> (ParseFailed value, Nothing)
            in (View (HMapCons viewEntry HMapNil), result)


text :: forall name. KnownSymbol name => Form '[ '(name, Text)] Text
text = Form go
  where
    go params =
        case List.lookup (T.pack $ symbolVal @name Proxy) params of
            Nothing -> (View (HMapCons Missing HMapNil), Nothing)
            Just value -> (View (HMapCons (Parsed value value) HMapNil), Just value)

-- -------------------
-- -- Forms --
-- -------------------
--
-- data (.:) (name :: Symbol) a = Jup
--
--
-- newtype (:->) (name :: Symbol) a = (:->) a
--
-- type Params = [(Text, Text)]
--
--
-- data FormView (fields :: NonEmpty Type) where
--     FormViewSingle :: forall name a. Maybe Text -> Maybe a -> FormView ((name :-> a) ':| '[])
--     FormView :: forall name a r1 rest. Maybe Text -> Maybe a -> FormView (r1 ':| rest) -> FormView ((name :-> a) ':| r1 ': rest)
--
--
-- mapFormViewFirst :: (a -> b) -> FormView ((n :-> a) ':| rest) -> FormView ((n :-> b) ':| rest)
-- mapFormViewFirst f (FormViewSingle input a) = FormViewSingle input (f <$> a)
-- mapFormViewFirst f (FormView input a rest) = FormView input (f <$> a) rest
--
--
-- data Form (fields :: NonEmpty Type) a where
--     Form :: KnownSymbol name => {runForm :: Maybe Params -> (FormView ((name :-> a) ':| rest), Maybe a)} -> Form a
--
--
-- instance Functor Form where
--     fmap :: forall a b. (a -> b) -> Form a -> Form b
--     fmap fun Form{..} =
--         Form
--             { runForm = \params ->
--                 let (view, maybeResult) = runForm params
--                  in (mapFormViewFirst fun view, fun <$> maybeResult)
--             }
--
--
-- -- instance Applicative Form where
-- --     pure = Form . const . Just
-- --     Form{runForm = run1} <*> Form{runForm = run2} =
-- --         Form
-- --             { runForm = \params -> run1 params <*> run2 params
-- --             }
--
-- getFieldParam :: forall (n :: Symbol). KnownSymbol n => Params -> Maybe Text
-- getFieldParam = lookup (T.pack $ symbolVal @n Proxy)
--
--
-- ----------------------
-- --- Fields ---
-- ----------------------
--
-- text :: forall (name :: Symbol). KnownSymbol name => Form Text
-- text =
--     let runForm :: Maybe Params -> (FormView (name :-> Text ':| '[]), Maybe Text)
--         runForm mParams = case mParams of
--             Nothing -> (FormViewSingle @name Nothing Nothing, Nothing)
--             Just params ->
--                 let value = getFieldParam @name params
--                  in (FormViewSingle @name value value, value)
--      in Form{..}
