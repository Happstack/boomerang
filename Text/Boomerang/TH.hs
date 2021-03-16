{-# LANGUAGE CPP, TemplateHaskell, TypeOperators #-}
module Text.Boomerang.TH
    ( makeBoomerangs
    -- * Backwards-compatibility
    , derivePrinterParsers
    ) where

import Control.Monad       (liftM, replicateM)
import Language.Haskell.TH
import Language.Haskell.TH.Datatype.TyVarBndr
import Text.Boomerang.HStack   ((:-)(..), arg)
import Text.Boomerang.Prim    (xpure, Boomerang)

-- | Make a 'Boomerang' router for each constructor in a datatype. For
-- example:
--
--   @$(makeBoomerangs \'\'Sitemap)@
makeBoomerangs :: Name -> Q [Dec]
makeBoomerangs name = do
  info <- reify name
  case info of
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (DataD _ tName tBinds _ cons _)   ->
#else
    TyConI (DataD _ tName tBinds cons _)   ->
#endif
      concat `liftM` mapM (deriveBoomerang (tName, tBinds)) cons
#if MIN_VERSION_template_haskell(2,11,0)
    TyConI (NewtypeD _ tName tBinds _ con _) ->
#else
    TyConI (NewtypeD _ tName tBinds con _) ->
#endif
      deriveBoomerang (tName, tBinds) con
    _ ->
      fail $ show name ++ " is not a datatype."

-- | Old name for 'makeBoomerangs', since renamed to reflect the fact
-- that it's not actually deriving instances for any type class, but rather
-- generates top-level definitions for routers of type 'Boomerang'.
derivePrinterParsers :: Name -> Q [Dec]
derivePrinterParsers = makeBoomerangs
{-# DEPRECATED derivePrinterParsers "Use makeBoomerangs instead" #-}

-- Derive a router for a single constructor.
deriveBoomerang :: (Name, [TyVarBndrUnit]) -> Con -> Q [Dec]
deriveBoomerang (tName, tParams) con =
  case con of
    NormalC name tys -> go name (map snd tys)
    RecC name tys -> go name (map (\(_,_,ty) -> ty) tys)
    _ -> do
      runIO $ putStrLn $ "Skipping unsupported constructor " ++ show (conName con)
      return []
  where
    go name tys = do
      let name' = mkBoomerangName name
      let tok' = mkName "tok"
      let e' = mkName "e"
      let ppType = AppT (AppT (ConT ''Boomerang) (VarT e')) (VarT tok')
      let r' = mkName "r"
      let inT = foldr (\a b -> AppT (AppT (ConT ''(:-)) a) b) (VarT r') tys
      let outT = AppT (AppT (ConT ''(:-))
                            (foldl AppT (ConT tName) (map (VarT . tvName) tParams)))
                      (VarT r')
      -- runIO $ putStrLn $ "Introducing router " ++ nameBase name' ++ "."
      expr <- [| xpure $(deriveConstructor name (length tys))
                     $(deriveDestructor name tys) |]
      return [ SigD name'
                    (ForallT (map plainTVSpecified ([tok', e', r'] ++ (map tvName tParams)))
                             []
                             (AppT (AppT ppType inT) outT))
             , FunD name' [Clause [] (NormalB expr) []]
             ]


-- Derive the contructor part of a router.
deriveConstructor :: Name -> Int -> Q Exp
deriveConstructor name arity = [| $(mk arity) $(conE name) |]
  where
    mk :: Int -> ExpQ
    mk 0 = [| (:-) |]
    mk n = [| arg $(mk (n - 1)) |]


-- Derive the destructor part of a router.
deriveDestructor :: Name -> [Type] -> Q Exp
deriveDestructor name tys = do
  -- Introduce some names
  x          <- newName "x"
  r          <- newName "r"
  fieldNames <- replicateM (length tys) (newName "a")

  -- Figure out the names of some constructors
  nothing    <- [| Nothing |]
  ConE just  <- [| Just |]
  ConE left  <- [| Left |]
  ConE right <- [| Right |]
  ConE cons  <- [| (:-) |]


  let conPat   = ConP name (map VarP fieldNames)
  let okBody   = ConE just `AppE`
                  foldr
                    (\h t -> ConE cons `AppE` VarE h `AppE` t)
                    (VarE r)
                    fieldNames
  let okCase   = Match (ConP cons [conPat, VarP r]) (NormalB okBody) []
  let nStr = show name
  let failCase = Match WildP (NormalB nothing) []

  return $ LamE [VarP x] (CaseE (VarE x) [okCase, failCase])


-- Derive the name of a router based on the name of the constructor in question.
mkBoomerangName :: Name -> Name
mkBoomerangName name = mkName ('r' : nameBase name)


-- Retrieve the name of a constructor.
conName :: Con -> Name
conName con =
  case con of
    NormalC name _  -> name
    RecC name _     -> name
    InfixC _ name _ -> name
    ForallC _ _ con' -> conName con'
