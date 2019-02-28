{-# LANGUAGE FlexibleContexts, ScopedTypeVariables, TypeOperators, TypeFamilies #-}
module Text.Boomerang.Prim
    ( -- * Types
    Parser(..), Boomerang(..), PrinterParser, (.~)
    -- * Running routers
    , parse, parse1, unparse, unparse1, bestErrors
    -- * Constructing / Manipulating Boomerangs
    , xpure, val, xmap
    -- heterogeneous list functions
    , xmaph
    ) where

import Prelude             hiding ((.), id)
import Control.Arrow       (first)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Category    (Category((.), id))
import Control.Monad       (MonadPlus(mzero, mplus), ap)
import Control.Monad.Error (Error(..))
import Data.Either         (partitionEithers)
import Data.Function       (on)
import Data.List           (partition)
import Data.Monoid         (Monoid(mappend, mempty))
import qualified Data.Semigroup as SG
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

instance Applicative (Parser e tok) where
    pure  = return
    (<*>) = ap

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

instance Alternative (Parser e tok) where
    empty = mzero
    (<|>) = mplus

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

-- | A @Boomerang a b@ takes an @a@ to parse a URL and results in @b@ if parsing succeeds.
--   And it takes a @b@ to serialize to a URL and results in @a@ if serializing succeeds.
data Boomerang e tok a b = Boomerang
  { prs :: Parser e tok (a -> b)
  , ser :: b -> [(tok -> tok, a)]
  }

type PrinterParser = Boomerang
{-# DEPRECATED PrinterParser "Use Boomerang instead" #-}

instance Category (Boomerang e tok) where
  id = Boomerang
    (return id)
    (\x -> [(id, x)])

  ~(Boomerang pf sf) . ~(Boomerang pg sg) = Boomerang
    (composeP (.) pf pg)
    (compose (.) sf sg)

instance SG.Semigroup (Boomerang e tok a b) where
  ~(Boomerang pf sf) <> ~(Boomerang pg sg) = Boomerang
    (pf `mplus` pg)
    (\s -> sf s `mplus` sg s)

instance Monoid (Boomerang e tok a b) where
  mempty = Boomerang
    mzero
    (const mzero)
  mappend = (SG.<>)

infixr 9 .~
-- | Reverse composition, but with the side effects still in left-to-right order.
(.~) :: Boomerang e tok a b -> Boomerang e tok b c -> Boomerang e tok a c
~(Boomerang pf sf) .~ ~(Boomerang pg sg) = Boomerang
  (composeP (flip (.)) pf pg)
  (compose (flip (.)) sg sf)

-- | Map over routers.
xmap :: (a -> b) -> (b -> Maybe a) -> Boomerang e tok r a -> Boomerang e tok r b
xmap f g (Boomerang p s) = Boomerang p' s'
    where
      p' = fmap (fmap f) p
      s' url = maybe mzero s (g url)

-- | Lift a constructor-destructor pair to a pure router.
xpure :: (a -> b) -> (b -> Maybe a) -> Boomerang e tok a b
xpure f g = xmap f g id

-- | Like "xmap", but only maps over the top of the stack.
xmaph :: (a -> b) -> (b -> Maybe a) -> Boomerang e tok i (a :- o) -> Boomerang e tok i (b :- o)
xmaph f g = xmap (hdMap f) (hdTraverse g)

-- | lift a 'Parser' and a printer into a 'Boomerang'
val :: forall e tok a r. Parser e tok a -> (a -> [tok -> tok]) -> Boomerang e tok r (a :- r)
val rs ss = Boomerang rs' ss'
    where
      rs' :: Parser e tok (r -> (a :- r))
      rs' = fmap (:-) rs
      ss' =  (\(a :- r) -> map (\f -> (f, r)) (ss a))

-- | Give all possible parses or errors.
parse :: forall e a p tok. (InitialPosition e) => Boomerang e tok () a -> tok -> [Either e (a, tok, Pos e)]
parse p s =
    map (either Left (\((f, tok), pos) -> Right (f (), tok, pos))) $ runParser (prs p) s (initialPos (Nothing :: Maybe e))

-- | Give the first parse, for Boomerangs with a parser that yields just one value.
-- Otherwise return the error (or errors) with the highest error position.
parse1 :: (Error e, ErrorPosition e, InitialPosition e, Show e, Ord (Pos e), Show tok ) => (tok -> Bool) -> Boomerang e tok () (a :- ()) -> tok -> Either [e] a
parse1 isComplete p s = 
  case partition (either (const False) (\(_,tok,_) -> isComplete tok )) $
       parse p s of
    ( [],             badParsings )  -> bestMsgs Nothing [] badParsings 
    ( Right ( u :- (), _, _ ):_, _) ->  Right u
   where 
    bestMsgs _      errMsgs []     = Left errMsgs
    bestMsgs errPos errMsgs (x:xs) = case x of 
      Right ( u :- ()  ,tok, pos) 
        | justPos > errPos -> bestMsgs justPos (newErrMsg :[]) xs  
        | otherwise        -> bestMsgs errPos   errMsgs        xs 
          where 
            justPos   = Just pos
            newErrMsg = setPosition pos  $ strMsg $ 
                        "no parse starting at " ++ (take 10 $ show tok) 
      Left e  -> let mPos = getPosition e in   
          case compare mPos errPos of 
            GT -> bestMsgs mPos   [e]         xs
            EQ -> bestMsgs mPos   (e:errMsgs) xs 
            LT -> bestMsgs errPos    errMsgs  xs 


-- | Give all possible serializations.
unparse :: tok -> Boomerang e tok () url -> url -> [tok]
unparse tok p = (map (($ tok) . fst)) . ser p

-- | Give the first serialization, for Boomerangs with a serializer that needs just one value.
unparse1 :: tok -> Boomerang e tok () (a :- ()) -> a -> Maybe tok
unparse1 tok p a =
    case unparse tok p (a :- ()) of
      [] -> Nothing
      (s:_) -> Just s
