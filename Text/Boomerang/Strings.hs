-- | a 'PrinterParser' library for working with '[String]'
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeSynonymInstances, TypeOperators #-}
module Text.Boomerang.Strings
    (
    -- * Types
      StringsError
    -- * Combinators
    , (</>), alpha, anyChar, anyString, char, digit, eos, int
    , integer, lit, readshow, satisfy, satisfyStr, space
    -- * Running the 'PrinterParser'
    , isComplete, parseStrings, unparseStrings

    )
    where

import Prelude                 hiding ((.), id, (/))
import Control.Category        (Category((.), id))
import Data.Char               (isAlpha, isDigit, isSpace)
import Data.Data               (Data, Typeable)
import Data.List               (stripPrefix)
import Data.String             (IsString(..))
import Text.Boomerang.Combinators (opt, rCons, rList1)
import Text.Boomerang.Error       (ParserError(..),ErrorMsg(..), (<?>), condenseErrors, mkParserError)
import Text.Boomerang.HStack       ((:-)(..))
import Text.Boomerang.Pos         (InitialPosition(..), MajorMinorPos(..), incMajor, incMinor)
import Text.Boomerang.Prim        (Parser(..), PrinterParser(..), parse1, xmaph, unparse1, val)

type StringsError = ParserError MajorMinorPos

instance InitialPosition StringsError where
    initialPos _ = MajorMinorPos 0 0

instance a ~ b => IsString (PrinterParser StringsError [String] a b) where
    fromString = lit

-- | a constant string
lit :: String -> PrinterParser StringsError [String] r r
lit l = PrinterParser pf sf
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> mkParserError pos [EOI "input", Expect (show l)]
             ("":_) | (not $ null l) -> mkParserError pos [EOI "segment", Expect (show l)]
             (p:ps) ->
                 case stripPrefix l p of
                   (Just p') ->
                       do [Right ((id, p':ps), incMinor (length l) pos)]
                   Nothing ->
                       mkParserError pos [UnExpect (show p), Expect (show l)]
      sf b = [ (\strings -> case strings of [] -> [l] ; (s:ss) -> ((l ++ s) : ss), b)]

infixr 9 </>
-- | equivalent to @f . eos . g@
(</>) :: PrinterParser StringsError [String] b c -> PrinterParser StringsError [String] a b -> PrinterParser StringsError [String] a c
f </> g = f . eos . g

-- | end of string
eos :: PrinterParser StringsError [String] r r
eos = PrinterParser
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), incMajor 1 pos)]
--                   [] -> mkParserError pos [EOI "input"]
                   ("":ps) ->
                          [ Right ((id, ps), incMajor 1 pos) ]
                   (p:_) -> mkParserError pos [Message $ "path-segment not entirely consumed: " ++ p])
       (\a -> [(("" :), a)])

-- | statisfy a 'Char' predicate
satisfy :: (Char -> Bool) -> PrinterParser StringsError [String] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos ->
       case tok of
         []          -> mkParserError pos [EOI "input"]
         ("":ss)     -> mkParserError pos [EOI "segment"]
         ((c:cs):ss)
             | p c ->
                 do [Right ((c, cs : ss), incMinor 1 pos )]
             | otherwise ->
                 do mkParserError pos [SysUnExpect $ show c]
  )
  (\c -> [ \paths -> case paths of [] -> [[c]] ; (s:ss) -> ((c:s):ss) | p c ])


-- | satisfy a 'String' predicate.
--
-- Note: must match the entire remainder of the 'String' in this segment
satisfyStr :: (String -> Bool) -> PrinterParser StringsError [String] r (String :- r)
satisfyStr p = val
  (Parser $ \tok pos ->
       case tok of
         []          -> mkParserError pos [EOI "input"]
         ("":ss)     -> mkParserError pos [EOI "segment"]
         (s:ss)
             | p s ->
                 do [Right ((s, "":ss), incMajor 1 pos )]
             | otherwise ->
                 do mkParserError pos [SysUnExpect $ show s]
  )
  (\str -> [ \strings -> case strings of [] -> [str] ; (s:ss) -> ((str++s):ss) | p str ])


-- | ascii digits @\'0\'..\'9\'@
digit :: PrinterParser StringsError [String] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: PrinterParser StringsError [String] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

-- | matches white-space characters in the Latin-1 range. (Uses 'isSpace')
space :: PrinterParser StringsError [String] r (Char :- r)
space = satisfy isSpace <?> "a white-space character"

-- | any character
anyChar :: PrinterParser StringsError [String] r (Char :- r)
anyChar = satisfy (const True)

-- | matches the specified character
char :: Char -> PrinterParser StringsError [String] r (Char :- r)
char c = satisfy (== c) <?> show [c]

-- | lift 'Read'/'Show' to a 'PrinterParser'
--
-- There are a few restrictions here:
--
--  1. Error messages are a bit fuzzy. `Read` does not tell us where
--  or why a parse failed. So all we can do it use the the position
--  that we were at when we called read and say that it failed.
--
--  2. it is (currently) not safe to use 'readshow' on integral values
--  because the 'Read' instance for 'Int', 'Integer', etc,
readshow :: (Read a, Show a) => PrinterParser StringsError [String] r (a :- r)
readshow =
    val readParser s
    where
      s a = [ \strings -> case strings of [] -> [show a] ; (s:ss) -> (((show a)++s):ss) ]

readParser :: (Read a) => Parser StringsError [String] a
readParser =
    Parser $ \tok pos ->
        case tok of
          []     -> mkParserError pos [EOI "input"]
          ("":_) -> mkParserError pos [EOI "segment"]
          (p:ps) ->
            case reads p of
              [] -> mkParserError pos [SysUnExpect p, Message $ "decoding using 'read' failed."]
              [(a,r)] ->
                  [Right ((a, r:ps), incMinor ((length p) - (length r)) pos)]

readIntegral :: (Read a, Eq a, Num a) => String -> a
readIntegral s =
    case reads s of
      [(x, [])] -> x
      []  -> error "readIntegral: no parse"
      _   -> error "readIntegral: ambiguous parse"


-- | matches an 'Int'
--
-- Note that the combinator @(rPair . int . int)@ is ill-defined because the parse can not tell where it is supposed to split the sequence of digits to produced two ints.
int :: PrinterParser StringsError [String] r (Int :- r)
int = xmaph readIntegral (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches an 'Integer'
--
-- Note that the combinator @(rPair . integer . integer)@ is ill-defined because the parse can not tell where it is supposed to split the sequence of digits to produced two ints.
integer :: PrinterParser StringsError [String] r (Integer :- r)
integer = xmaph readIntegral (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches any 'String'
--
-- the parser returns the remainder of the current String segment, (but does not consume the 'end of segment'.
--
-- Note that the only combinator that should follow 'anyString' is
-- 'eos' or '</>'. Other combinators will lead to inconsistent
-- inversions.
--
-- For example, if we have:
--
-- > unparseStrings (rPair . anyString . anyString)  ("foo","bar")
--
-- That will unparse to @Just ["foobar"]@. But if we call
--
-- > parseStrings (rPair . anyString . anyString)  ["foobar"]
--
-- We will get @Right ("foobar","")@ instead of the original @Right ("foo","bar")@
anyString :: PrinterParser StringsError [String] r (String :- r)
anyString = val ps ss
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> mkParserError pos [EOI "input", Expect "any string"]
--             ("":_) -> mkParserError pos [EOI "segment", Expect "any string"]
             (s:ss) -> [Right ((s, "":ss), incMinor (length s) pos)]
      ss str = [\ss -> case ss of
                         []      -> [str]
                         (s:ss') -> ((str ++ s) : ss')
               ]

-- | Predicate to test if we have parsed all the strings.
-- Typically used as argument to 'parse1'
--
-- see also: 'parseStrings'
isComplete :: [String] -> Bool
isComplete []   = True
isComplete [""] = True
isComplete _    = False

-- | run the parser
--
-- Returns the first complete parse or a parse error.
--
-- > parseStrings (rUnit . lit "foo") ["foo"]
parseStrings :: PrinterParser StringsError [String] () (r :- ())
             -> [String]
             -> Either StringsError r
parseStrings pp strs =
    either (Left . condenseErrors) Right $ parse1 isComplete pp strs

-- | run the printer
--
-- > unparseStrings (rUnit . lit "foo") ()
unparseStrings :: PrinterParser e [String] () (r :- ()) -> r -> Maybe [String]
unparseStrings pp r = unparse1 [] pp r