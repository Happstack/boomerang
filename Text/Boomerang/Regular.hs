{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Web.Boomerang.Regular 
  ( mkPrinterParsers
  , PrinterParsers
  , PrinterParserList(..)
  
  -- * Re-exported from Generics.Regular
  , deriveAll
  , PF
  ) where

import Text.Boomerang.Prim
import Text.Boomerang.HStack
import Generics.Regular

infixr :&

-- | The type of the list of routers generated for type @r@.
type PrinterParsers e tok r = PrinterParserList e tok (PF r) r

-- | Creates the routers for type @r@, one for each constructor. For example:
--
--   @Z rHome :& Z rUserOverview :& Z rUserDetail :& Z rArticle = mkPrinterParsers@
mkPrinterParsers :: (MkPrinterParsers (PF r), Regular r) => PrinterParsers e tok r
mkPrinterParsers = mkPrinterParsers' to (Just . from)

data family PrinterParserList e tok f r
class MkPrinterParsers (f :: * -> *) where
  mkPrinterParsers' :: (f r -> r) -> (r -> Maybe (f r)) -> PrinterParserList e tok f r

data instance PrinterParserList e tok (C c f) r = Z (forall t. PrinterParser e tok (PrinterParserLhs f r t) (r :- t))
instance MkPrinterParser f => MkPrinterParsers (C c f) where
  mkPrinterParsers' addLR matchLR = Z $ xpure (hdMap (addLR . C) . mkP) (fmap mkS . hdTraverse (fmap unC . matchLR))

data instance PrinterParserList e tok (f :+: g) r = PrinterParserList e tok f r :& PrinterParserList e tok g r
instance (MkPrinterParsers f, MkPrinterParsers g) => MkPrinterParsers (f :+: g) where
  mkPrinterParsers' addLR matchLR = mkPrinterParsers' (addLR . L) (matchL matchLR) 
                          :& mkPrinterParsers' (addLR . R) (matchR matchLR)
    where
      matchL :: (r ->  Maybe ((f :+: g) r)) -> r -> Maybe (f r)
      matchL frm r = case frm r of 
        Just (L f) -> Just f
        _ -> Nothing

      matchR :: (r -> Maybe ((f :+: g) r)) -> r -> Maybe (g r)
      matchR frm r = case frm r of 
        Just (R f) -> Just f
        _ -> Nothing


type family PrinterParserLhs (f :: * -> *) (r :: *) (t :: *) :: *
class MkPrinterParser (f :: * -> *) where
  mkP :: PrinterParserLhs f r t -> (f r :- t)
  mkS :: (f r :- t) -> PrinterParserLhs f r t

type instance PrinterParserLhs U r t = t
instance MkPrinterParser U where
  mkP t = U :- t
  mkS (U :- r) = r

type instance PrinterParserLhs (K a) r t = a :- t
instance MkPrinterParser (K a) where
  mkP (a :- t) = K a :- t
  mkS (K a :- t) = a :- t

type instance PrinterParserLhs I r t = r :- t
instance MkPrinterParser I where
  mkP (r :- t) = I r :- t
  mkS (I r :- t) = r :- t

type instance PrinterParserLhs (f :*: g) r t = PrinterParserLhs f r (PrinterParserLhs g r t)
instance (MkPrinterParser f, MkPrinterParser g) => MkPrinterParser (f :*: g) where
  mkP t = (f :*: g) :- t''
    where 
      f :- t'  = mkP t
      g :- t'' = mkP t'
  mkS ((f :*: g) :- t) = mkS (f :- mkS (g :- t))
