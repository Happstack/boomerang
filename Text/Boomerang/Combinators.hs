-- | a collection of generic parsing combinators that can work with any token and error type.
{-# LANGUAGE CPP, TemplateHaskell, TypeOperators #-}
module Text.Boomerang.Combinators
    ( (<>), duck, duck1, opt
    , manyr, somer, chainr, chainr1, manyl, somel, chainl, chainl1
    , rFilter, printAs, push, rNil, rCons, rList, rList1, rListSep, rPair
    , rLeft, rRight, rEither, rNothing, rJust, rMaybe
    , rTrue, rFalse, rBool, rUnit
    )
    where

import Control.Arrow       (first, second)
import Prelude             hiding ((.), id, (/))
import Control.Category    (Category((.), id))
import Control.Monad       (guard)
import Control.Monad.Error (Error)
import Text.Boomerang.Prim    (Parser(..), Boomerang(..), (.~), val, xpure)
import Text.Boomerang.HStack   ((:-)(..), arg, hhead)
import Text.Boomerang.TH      (makeBoomerangs)

#if MIN_VERSION_base(4,5,0)
import Data.Monoid         (Monoid(mappend), (<>))
#else
import Data.Monoid         (Monoid(mappend))

infixr 6 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- | Convert a router to do what it does on the tail of the stack.
duck :: Boomerang e tok r1 r2 -> Boomerang e tok (h :- r1) (h :- r2)
duck r = Boomerang
  (fmap (\f (h :- t) -> h :- f t) $ prs r)
  (\(h :- t) -> map (second (h :-)) $ ser r t)

-- | Convert a router to do what it does on the tail of the stack.
duck1 :: Boomerang e tok r1 (a :- r2) -> Boomerang e tok (h :- r1) (a :- h :- r2)
duck1 r = Boomerang
  (fmap (\f (h :- t) -> let a :- t' = f t in a :- h :- t') $ prs r)
  (\(a :- h :- t) -> map (second (h :-)) $ ser r (a :- t))

-- | Make a router optional.
opt :: Boomerang e tok r r -> Boomerang e tok r r
opt = (id <>)

-- | Repeat a router zero or more times, combining the results from left to right.
manyr :: Boomerang e tok r r -> Boomerang e tok r r
manyr = opt . somer

-- | Repeat a router one or more times, combining the results from left to right.
somer :: Boomerang e tok r r -> Boomerang e tok r r
somer p = p . manyr p

-- | @chainr p op@ repeats @p@ zero or more times, separated by @op@.
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr :: Boomerang e tok r r -> Boomerang e tok r r -> Boomerang e tok r r
chainr p op = opt (manyr (p .~ op) . p)

-- | @chainr1 p op@ repeats @p@ one or more times, separated by @op@.
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr1 :: Boomerang e tok r (a :- r) -> Boomerang e tok (a :- a :- r) (a :- r) -> Boomerang e tok r (a :- r)
chainr1 p op = manyr (duck p .~ op) . p

-- | Repeat a router zero or more times, combining the results from right to left.
manyl :: Boomerang e tok r r -> Boomerang e tok r r
manyl = opt . somel

-- | Repeat a router one or more times, combining the results from right to left.
somel :: Boomerang e tok r r -> Boomerang e tok r r
somel p = p .~ manyl p

-- | @chainl1 p op@ repeats @p@ zero or more times, separated by @op@.
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl :: Boomerang e tok r r -> Boomerang e tok r r -> Boomerang e tok r r
chainl p op = opt (p .~ manyl (op . p))

-- | @chainl1 p op@ repeats @p@ one or more times, separated by @op@.
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl1 :: Boomerang e tok r (a :- r) -> Boomerang e tok (a :- a :- r) (a :- r) -> Boomerang e tok r (a :- r)
chainl1 p op = p .~ manyl (op . duck p)

-- | Filtering on routers.
--
-- TODO: We remove any parse errors, not sure if the should be preserved. Also, if the predicate fails we silently remove the element, but perhaps we should replace the value with an error message?
rFilter :: (a -> Bool) -> Boomerang e tok () (a :- ()) -> Boomerang e tok r (a :- r)
rFilter p r = val ps ss
    where
      ps = Parser $ \tok pos ->
           let parses = runParser (prs r) tok pos
           in [ Right ((a, tok), pos) | (Right ((f, tok), pos)) <- parses, let a = hhead (f ()), p a]
      ss = \a -> [ f | p a, (f, _) <- ser r (a :- ()) ]

-- | @r \`printAs\` s@ uses ther serializer of @r@ to test if serializing succeeds,
--   and if it does, instead serializes as @s@.
--
-- TODO: can this be more general so that it can work on @tok@ instead of @[tok]@
printAs :: Boomerang e [tok] a b -> tok -> Boomerang e [tok] a b
printAs r s = r { ser = map (first (const (s :))) . take 1 . ser r }

-- | Push a value on the stack (during parsing, pop it from the stack when serializing).
push :: (Eq a, Error e) => a -> Boomerang e tok r (a :- r)
push a = xpure (a :-) (\(a' :- t) -> guard (a' == a) >> Just t)

rNil :: Boomerang e tok r ([a] :- r)
rNil = xpure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: Boomerang e tok (a :- [a] :- r) ([a] :- r)
rCons = xpure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList :: Boomerang e tok r (a :- r) -> Boomerang e tok r ([a] :- r)
rList r = manyr (rCons . duck1 r) . rNil

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList1 :: Boomerang e tok r (a :- r) -> Boomerang e tok r ([a] :- r)
rList1 r = somer (rCons . duck1 r) . rNil

-- | Converts a router for a value @a@ to a router for a list of @a@, with a separator.
rListSep :: Boomerang e tok r (a :- r) -> Boomerang e tok ([a] :- r) ([a] :- r) -> Boomerang e tok r ([a] :- r)
rListSep r sep = chainr (rCons . duck1 r) sep . rNil

rPair :: Boomerang e tok (f :- s :- r) ((f, s) :- r)
rPair = xpure (arg (arg (:-)) (,)) $ \(ab :- t) -> do (a,b) <- Just ab; Just (a :- b :- t)

$(makeBoomerangs ''Either)

-- | Combines a router for a value @a@ and a router for a value @b@ into a router for @Either a b@.
rEither :: Boomerang e tok r (a :- r) -> Boomerang e tok r (b :- r) -> Boomerang e tok r (Either a b :- r)
rEither l r = rLeft . l <> rRight . r

$(makeBoomerangs ''Maybe)

-- | Converts a router for a value @a@ to a router for a @Maybe a@.
rMaybe :: Boomerang e tok r (a :- r) -> Boomerang e tok r (Maybe a :- r)
rMaybe r = rJust . r <> rNothing

$(makeBoomerangs ''Bool)

rBool :: Boomerang e tok a r -- ^ 'True' parser
      -> Boomerang e tok a r -- ^ 'False' parser
      -> Boomerang e tok a (Bool :- r)
rBool t f = rTrue . t <> rFalse . f

rUnit :: Boomerang e tok r (() :- r)
rUnit = xpure ((:-) ()) (\ (() :- x) -> Just x)
