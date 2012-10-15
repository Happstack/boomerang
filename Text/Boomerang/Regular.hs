{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Boomerang.Regular
  ( mkBoomerangs
  , Boomerangs
  , BoomerangList(..)

  -- * Re-exported from Generics.Regular
  , deriveAll
  , PF
  ) where

import Text.Boomerang.Prim
import Text.Boomerang.HStack
import Generics.Regular

infixr :&

-- | The type of the list of routers generated for type @r@.
type Boomerangs e tok r = BoomerangList e tok (PF r) r

-- | Creates the routers for type @r@, one for each constructor. For example:
--
--   @Z rHome :& Z rUserOverview :& Z rUserDetail :& Z rArticle = mkBoomerangs@
mkBoomerangs :: (MkBoomerangs (PF r), Regular r) => Boomerangs e tok r
mkBoomerangs = mkBoomerangs' to (Just . from)

data family BoomerangList e tok f r
class MkBoomerangs (f :: * -> *) where
  mkBoomerangs' :: (f r -> r) -> (r -> Maybe (f r)) -> BoomerangList e tok f r

data instance BoomerangList e tok (C c f) r = Z (forall t. Boomerang e tok (BoomerangLhs f r t) (r :- t))
instance MkBoomerang f => MkBoomerangs (C c f) where
  mkBoomerangs' addLR matchLR = Z $ xpure (hdMap (addLR . C) . mkP) (fmap mkS . hdTraverse (fmap unC . matchLR))

data instance BoomerangList e tok (f :+: g) r = BoomerangList e tok f r :& BoomerangList e tok g r
instance (MkBoomerangs f, MkBoomerangs g) => MkBoomerangs (f :+: g) where
  mkBoomerangs' addLR matchLR = mkBoomerangs' (addLR . L) (matchL matchLR)
                          :& mkBoomerangs' (addLR . R) (matchR matchLR)
    where
      matchL :: (r ->  Maybe ((f :+: g) r)) -> r -> Maybe (f r)
      matchL frm r = case frm r of
        Just (L f) -> Just f
        _ -> Nothing

      matchR :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (g r)
      matchR frm r = case frm r of
        Just (R f) -> Just f
        _ -> Nothing


type family BoomerangLhs (f :: * -> *) (r :: *) (t :: *) :: *
class MkBoomerang (f :: * -> *) where
  mkP :: BoomerangLhs f r t -> (f r :- t)
  mkS :: (f r :- t) -> BoomerangLhs f r t

type instance BoomerangLhs U r t = t
instance MkBoomerang U where
  mkP t = U :- t
  mkS (U :- r) = r

type instance BoomerangLhs (K a) r t = a :- t
instance MkBoomerang (K a) where
  mkP (a :- t) = K a :- t
  mkS (K a :- t) = a :- t

type instance BoomerangLhs I r t = r :- t
instance MkBoomerang I where
  mkP (r :- t) = I r :- t
  mkS (I r :- t) = r :- t

type instance BoomerangLhs (f :*: g) r t = BoomerangLhs f r (BoomerangLhs g r t)
instance (MkBoomerang f, MkBoomerang g) => MkBoomerang (f :*: g) where
  mkP t = (f :*: g) :- t''
    where
      f :- t'  = mkP t
      g :- t'' = mkP t'
  mkS ((f :*: g) :- t) = mkS (f :- mkS (g :- t))
