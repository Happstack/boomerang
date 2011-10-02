-- | a collection of generic parsing combinators that can work with any token and error type.
{-# LANGUAGE TemplateHaskell, TypeOperators #-}
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
import Data.Monoid         (Monoid(mappend))
import Text.Boomerang.Prim    (Parser(..), PrinterParser(..), (.~), val, xpure)
import Text.Boomerang.HStack   ((:-)(..), arg, hhead)
import Text.Boomerang.TH      (derivePrinterParsers)

infixr 8 <>

-- | Infix operator for 'mappend'.
(<>) :: Monoid m => m -> m -> m
(<>) = mappend

-- | Convert a router to do what it does on the tail of the stack.
duck :: PrinterParser e tok r1 r2 -> PrinterParser e tok (h :- r1) (h :- r2)
duck r = PrinterParser
  (fmap (\f (h :- t) -> h :- f t) $ prs r)
  (\(h :- t) -> map (second (h :-)) $ ser r t)

-- | Convert a router to do what it does on the tail of the stack.
duck1 :: PrinterParser e tok r1 (a :- r2) -> PrinterParser e tok (h :- r1) (a :- h :- r2)
duck1 r = PrinterParser
  (fmap (\f (h :- t) -> let a :- t' = f t in a :- h :- t') $ prs r)
  (\(a :- h :- t) -> map (second (h :-)) $ ser r (a :- t))

-- | Make a router optional.
opt :: PrinterParser e tok r r -> PrinterParser e tok r r
opt = (id <>)

-- | Repeat a router zero or more times, combining the results from left to right.
manyr :: PrinterParser e tok r r -> PrinterParser e tok r r
manyr = opt . somer

-- | Repeat a router one or more times, combining the results from left to right.
somer :: PrinterParser e tok r r -> PrinterParser e tok r r
somer p = p . manyr p

-- | @chainr p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr :: PrinterParser e tok r r -> PrinterParser e tok r r -> PrinterParser e tok r r
chainr p op = opt (manyr (p .~ op) . p)

-- | @chainr1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a right associative fold of the results of @p@ with the results of @op@.
chainr1 :: PrinterParser e tok r (a :- r) -> PrinterParser e tok (a :- a :- r) (a :- r) -> PrinterParser e tok r (a :- r)
chainr1 p op = manyr (duck p .~ op) . p

-- | Repeat a router zero or more times, combining the results from right to left.
manyl :: PrinterParser e tok r r -> PrinterParser e tok r r
manyl = opt . somel

-- | Repeat a router one or more times, combining the results from right to left.
somel :: PrinterParser e tok r r -> PrinterParser e tok r r
somel p = p .~ manyl p

-- | @chainl1 p op@ repeats @p@ zero or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl :: PrinterParser e tok r r -> PrinterParser e tok r r -> PrinterParser e tok r r
chainl p op = opt (p .~ manyl (op . p))

-- | @chainl1 p op@ repeats @p@ one or more times, separated by @op@. 
--   The result is a left associative fold of the results of @p@ with the results of @op@.
chainl1 :: PrinterParser e tok r (a :- r) -> PrinterParser e tok (a :- a :- r) (a :- r) -> PrinterParser e tok r (a :- r)
chainl1 p op = p .~ manyl (op . duck p)

-- | Filtering on routers.
-- 
-- TODO: We remove any parse errors, not sure if the should be preserved. Also, if the predicate fails we silently remove the element, but perhaps we should replace the value with an error message?
rFilter :: (a -> Bool) -> PrinterParser e tok () (a :- ()) -> PrinterParser e tok r (a :- r) 
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
printAs :: PrinterParser e [tok] a b -> tok -> PrinterParser e [tok] a b
printAs r s = r { ser = map (first (const (s :))) . take 1 . ser r }

-- | Push a value on the stack (during parsing, pop it from the stack when serializing).
push :: (Eq a, Error e) => a -> PrinterParser e tok r (a :- r)
push a = xpure (a :-) (\(a' :- t) -> guard (a' == a) >> Just t)

rNil :: PrinterParser e tok r ([a] :- r)
rNil = xpure ([] :-) $ \(xs :- t) -> do [] <- Just xs; Just t

rCons :: PrinterParser e tok (a :- [a] :- r) ([a] :- r)
rCons = xpure (arg (arg (:-)) (:)) $ \(xs :- t) -> do a:as <- Just xs; Just (a :- as :- t)

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList :: PrinterParser e tok r (a :- r) -> PrinterParser e tok r ([a] :- r)
rList r = manyr (rCons . duck1 r) . rNil

-- | Converts a router for a value @a@ to a router for a list of @a@.
rList1 :: PrinterParser e tok r (a :- r) -> PrinterParser e tok r ([a] :- r)
rList1 r = somer (rCons . duck1 r) . rNil

-- | Converts a router for a value @a@ to a router for a list of @a@, with a separator.
rListSep :: PrinterParser e tok r (a :- r) -> PrinterParser e tok ([a] :- r) ([a] :- r) -> PrinterParser e tok r ([a] :- r)
rListSep r sep = chainr (rCons . duck1 r) sep . rNil

rPair :: PrinterParser e tok (f :- s :- r) ((f, s) :- r)
rPair = xpure (arg (arg (:-)) (,)) $ \(ab :- t) -> do (a,b) <- Just ab; Just (a :- b :- t)

$(derivePrinterParsers ''Either)

-- | Combines a router for a value @a@ and a router for a value @b@ into a router for @Either a b@.
rEither :: PrinterParser e tok r (a :- r) -> PrinterParser e tok r (b :- r) -> PrinterParser e tok r (Either a b :- r)
rEither l r = rLeft . l <> rRight . r

$(derivePrinterParsers ''Maybe)

-- | Converts a router for a value @a@ to a router for a @Maybe a@.
rMaybe :: PrinterParser e tok r (a :- r) -> PrinterParser e tok r (Maybe a :- r)
rMaybe r = rJust . r <> rNothing

$(derivePrinterParsers ''Bool)

rBool :: PrinterParser e tok a r -- ^ 'True' parser
      -> PrinterParser e tok a r -- ^ 'False' parser
      -> PrinterParser e tok a (Bool :- r)
rBool t f = rTrue . t <> rFalse . f

rUnit :: PrinterParser e tok r (() :- r)
rUnit = xpure ((:-) ()) (\ (() :- x) -> Just x)
