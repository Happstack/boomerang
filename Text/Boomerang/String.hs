-- | a 'PrinterParser' library for working with a 'String'
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeSynonymInstances, TypeOperators #-}
module Text.Boomerang.String
    (
    -- * Types
      StringPrinterParser, StringError
    -- * Combinators
    , alpha, anyChar, char, digit, int
    , integer, lit, satisfy, space
    -- * Running the 'PrinterParser'
    , isComplete, parseString, unparseString
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

type StringError         = ParserError MajorMinorPos
type StringPrinterParser = PrinterParser StringError String

instance InitialPosition StringError where
    initialPos _ = MajorMinorPos 0 0


-- | a constant string
lit :: String -> StringPrinterParser r r
lit l = PrinterParser pf sf 
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> mkParserError pos [EOI "input", Expect (show l)]
             _  -> parseLit l tok pos
      sf b = [ (\string -> (l ++ string), b)]

parseLit :: String -> String -> MajorMinorPos -> [Either StringError ((r -> r, String), MajorMinorPos)]
parseLit [] ss pos         = [Right ((id, ss), pos)]
parseLit (l:_) [] pos      = mkParserError pos [EOI "input", Expect (show l)]
parseLit (l:ls) (s:ss) pos
    | l /= s    = mkParserError pos [UnExpect (show s), Expect (show l)]
    | otherwise = parseLit ls ss (if l == '\n' then incMajor 1 pos else incMinor 1 pos)

instance a ~ b => IsString (PrinterParser StringError String a b) where
    fromString = lit

-- | statisfy a 'Char' predicate
satisfy :: (Char -> Bool) -> StringPrinterParser r (Char :- r)
satisfy p = val
  (Parser $ \tok pos -> 
       case tok of
         []          -> mkParserError pos [EOI "input"]
         (c:cs)
             | p c -> 
                 do [Right ((c, cs), if (c == '\n') then incMajor 1 pos else incMinor 1 pos)]
             | otherwise -> 
                 do mkParserError pos [SysUnExpect $ show c]
  )
  (\c -> [ \paths -> (c:paths) | p c ])

-- | ascii digits @\'0\'..\'9\'@
digit :: StringPrinterParser r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters, 
-- plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: StringPrinterParser r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

-- | matches white-space characters in the Latin-1 range. (Uses 'isSpace')
space :: StringPrinterParser r (Char :- r)
space = satisfy isSpace <?> "a white-space character"

-- | any character
anyChar :: StringPrinterParser r (Char :- r)
anyChar = satisfy (const True)

-- | matches the specified character
char :: Char -> StringPrinterParser r (Char :- r)
char c = satisfy (== c) <?> show [c]

-- | matches an 'Int'
int :: StringPrinterParser r (Int :- r)
int = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | matches an 'Integer'
integer :: StringPrinterParser r (Integer :- r)
integer = xmaph read (Just . show) (opt (rCons . char '-') . (rList1 digit))

-- | Predicate to test if we have parsed all the strings.
-- Typically used as argument to 'parse1'
--
-- see also: 'parseStrings'
isComplete :: String -> Bool
isComplete = null

-- | run the parser
--
-- Returns the first complete parse or a parse error.
--
-- > parseString (rUnit . lit "foo") ["foo"]
parseString :: StringPrinterParser () (r :- ())
             -> String
             -> Either StringError r
parseString pp strs = 
    either (Left . condenseErrors) Right $ parse1 isComplete pp strs

-- | run the printer
--
-- > unparseString (rUnit . lit "foo") ()
unparseString :: StringPrinterParser () (r :- ()) -> r -> Maybe String
unparseString pp r = unparse1 [] pp r
