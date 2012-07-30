{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeOperators, TypeFamilies #-}
module Text.Boomerang.Prim
    ( -- * Types
    Parser(..), PrinterParser(..), (.~)
    -- * Running routers
    , parse, parse1, unparse, unparse1, bestErrors
    -- * Constructing / Manipulating PrinterParsers
    , xpure, val, xmap
    -- heterogeneous list functions
    , xmaph
    ) where

import Prelude             hiding ((.), id)
import Control.Arrow       (first)
import Control.Category    (Category((.), id))
import Control.Monad       (MonadPlus(mzero, mplus))
import Control.Monad.Error (Error(..))
import Data.Either         (partitionEithers)
import Data.Function       (on)
import Data.Monoid         (Monoid(mappend, mempty))
import Text.Boomerang.HStack   ((:-)(..), hdMap, hdTraverse)
import Text.Boomerang.Pos     (ErrorPosition(..), InitialPosition(..), Pos)

compose
  :: (a -> b -> c)
  -> (i -> [(a, j)])
  -> (j -> [(b, k)])
  -> (i -> [(c, k)])
compose op mf mg s = do
  (f, s') <- mf s
  (g, s'') <- mg s'
  return (f `op` g, s'')

-- | The 'maximumsBy' function takes a comparison function and a list
-- and returns the greatest elements of the list by the comparison function.
-- The list must be finite and non-empty.
maximumsBy               :: (a -> a -> Ordering) -> [a] -> [a]
maximumsBy _ []          =  error "Text.Boomerang.Core.maximumsBy: empty list"
maximumsBy cmp (x:xs)    =  foldl maxBy [x] xs
    where
      maxBy xs@(x:_) y =
          case cmp x y of
            GT -> xs
            EQ -> (y:xs)
            LT  -> [y]

-- |Yet another parser.
--
-- Returns all possible parses and parse errors
newtype Parser e tok a = Parser { runParser :: tok -> Pos e -> [Either e ((a, tok), Pos e)] }

instance Functor (Parser e tok) where
    fmap f (Parser p) =
        Parser $ \tok pos ->
            map (fmap (first (first f))) (p tok pos)

instance Monad (Parser e tok) where
    return a =
        Parser $ \tok pos ->
            [Right ((a, tok), pos)]
    (Parser p) >>= f =
        Parser $ \tok pos ->
            case partitionEithers (p tok pos) of
              ([], []) -> []
              (errs,[]) -> map Left errs
              (_,as) -> concat [ runParser (f a) tok' pos' | ((a, tok'), pos') <- as ]

instance MonadPlus (Parser e tok) where
    mzero = Parser $ \tok pos -> []
    (Parser x) `mplus` (Parser y) =
        Parser $ \tok pos ->
            (x tok pos) ++ (y tok pos)

composeP
  :: (a -> b -> c)
  -> Parser e tok a
  -> Parser e tok b
  -> Parser e tok c
composeP op mf mg =
    do f <- mf
       g <- mg
       return (f `op` g)

-- | Attempt to extract the most relevant errors from a list of parse errors.
--
-- The current heuristic is to find error (or errors) where the error position is highest.
bestErrors :: (ErrorPosition e, Ord (Pos e)) => [e] -> [e]
bestErrors [] = []
bestErrors errs = maximumsBy (compare `on` getPosition) errs

-- | A @PrinterParser a b@ takes an @a@ to parse a URL and results in @b@ if parsing succeeds.
--   And it takes a @b@ to serialize to a URL and results in @a@ if serializing succeeds.
data PrinterParser e tok a b = PrinterParser
  { prs :: Parser e tok (a -> b)
  , ser :: b -> [(tok -> tok, a)]
  }

instance Category (PrinterParser e tok) where
  id = PrinterParser
    (return id)
    (\x -> [(id, x)])

  ~(PrinterParser pf sf) . ~(PrinterParser pg sg) = PrinterParser
    (composeP (.) pf pg)
    (compose (.) sf sg)

instance Monoid (PrinterParser e tok a b) where
  mempty = PrinterParser
    mzero
    (const mzero)

  ~(PrinterParser pf sf) `mappend` ~(PrinterParser pg sg) = PrinterParser
    (pf `mplus` pg)
    (\s -> sf s `mplus` sg s)

infixr 9 .~
-- | Reverse composition, but with the side effects still in left-to-right order.
(.~) :: PrinterParser e tok a b -> PrinterParser e tok b c -> PrinterParser e tok a c
~(PrinterParser pf sf) .~ ~(PrinterParser pg sg) = PrinterParser
  (composeP (flip (.)) pf pg)
  (compose (flip (.)) sg sf)

-- | Map over routers.
xmap :: (a -> b) -> (b -> Maybe a) -> PrinterParser e tok r a -> PrinterParser e tok r b
xmap f g (PrinterParser p s) = PrinterParser p' s'
    where
      p' = fmap (fmap f) p
      s' url = maybe mzero s (g url)

-- | Lift a constructor-destructor pair to a pure router.
xpure :: (a -> b) -> (b -> Maybe a) -> PrinterParser e tok a b
xpure f g = xmap f g id

-- | Like "xmap", but only maps over the top of the stack.
xmaph :: (a -> b) -> (b -> Maybe a) -> PrinterParser e tok i (a :- o) -> PrinterParser e tok i (b :- o)
xmaph f g = xmap (hdMap f) (hdTraverse g)

-- | lift a 'Parser' and a printer into a 'PrinterParser'
val :: forall e tok a r. Parser e tok a -> (a -> [tok -> tok]) -> PrinterParser e tok r (a :- r)
val rs ss = PrinterParser rs' ss'
    where
      rs' :: Parser e tok (r -> (a :- r))
      rs' = fmap (:-) rs
      ss' =  (\(a :- r) -> map (\f -> (f, r)) (ss a))

-- | Give all possible parses or errors.
parse :: forall e a p tok. (InitialPosition e) => PrinterParser e tok () a -> tok -> [Either e (a, tok)]
parse p s =
    map (either Left (\((f, tok), _) -> Right (f (), tok))) $ runParser (prs p) s (initialPos (Nothing :: Maybe e))

-- | Give the first parse, for PrinterParsers with a parser that yields just one value.
-- Otherwise return the error (or errors) with the highest error position.
parse1 :: (ErrorPosition e, InitialPosition e, Show e, Ord (Pos e)) =>
          (tok -> Bool) -> PrinterParser e tok () (a :- ()) -> tok -> Either [e] a
parse1 isComplete r paths =
    let results = parse r paths
    in case [ a | (Right (a,tok)) <- results, isComplete tok ] of
         ((u :- ()):_) -> Right u
         _             -> Left $ bestErrors [ e | Left e <- results ]

-- | Give all possible serializations.
unparse :: tok -> PrinterParser e tok () url -> url -> [tok]
unparse tok p = (map (($ tok) . fst)) . ser p

-- | Give the first serialization, for PrinterParsers with a serializer that needs just one value.
unparse1 :: tok -> PrinterParser e tok () (a :- ()) -> a -> Maybe tok
unparse1 tok p a =
    case unparse tok p (a :- ()) of
      [] -> Nothing
      (s:_) -> Just s
