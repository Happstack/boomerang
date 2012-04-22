-- | a 'PrinterParser' library for working with '[Text]'
{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, TypeFamilies, TypeSynonymInstances, TypeOperators #-}
module Text.Boomerang.Texts
    (
    -- * Types
      TextsError
    -- * Combinators
    , (</>), alpha, anyChar, anyText, char, digit, digits, signed, eos, integral, int
    , integer, lit, readshow, satisfy, satisfyStr, space
    , rTextCons, rEmpty, rText, rText1
    -- * Running the 'PrinterParser'
    , isComplete, parseTexts, unparseTexts
    )
    where

import Prelude                    hiding ((.), id, (/))
import Control.Category           (Category((.), id))
import Data.Char                  (isAlpha, isDigit, isSpace)
import Data.String                (IsString(..))
import           Data.Text        (Text)
import qualified Data.Text        as Text
import qualified Data.Text.Read   as Text
import Text.Boomerang.Combinators (opt, duck1, manyr, somer)
import Text.Boomerang.Error       (ParserError(..),ErrorMsg(..), (<?>), condenseErrors, mkParserError)
import Text.Boomerang.HStack      ((:-)(..), arg)
import Text.Boomerang.Pos         (InitialPosition(..), MajorMinorPos(..), incMajor, incMinor)
import Text.Boomerang.Prim        (Parser(..), PrinterParser(..), parse1, xmaph, xpure, unparse1, val)

type TextsError = ParserError MajorMinorPos

instance InitialPosition TextsError where
    initialPos _ = MajorMinorPos 0 0

instance a ~ b => IsString (PrinterParser TextsError [Text] a b) where
    fromString = lit . Text.pack

-- | a constant string
lit :: Text -> PrinterParser TextsError [Text] r r
lit l = PrinterParser pf sf
    where
      pf = Parser $ \tok pos ->
           case tok of
             [] -> mkParserError pos [EOI "input", Expect (show l)]
             (p:ps)
                 | Text.null p && (not $ Text.null l) -> mkParserError pos [EOI "segment", Expect (show l)]
                 | otherwise ->
                     case Text.stripPrefix l p of
                       (Just p') ->
                           [Right ((id, p':ps), incMinor (Text.length l) pos)]
                       Nothing ->
                           mkParserError pos [UnExpect (show p), Expect (show l)]
      sf b = [ (\strings -> case strings of [] -> [l] ; (s:ss) -> ((l `Text.append` s) : ss), b)]

infixr 9 </>
-- | equivalent to @f . eos . g@
(</>) :: PrinterParser TextsError [Text] b c -> PrinterParser TextsError [Text] a b -> PrinterParser TextsError [Text] a c
f </> g = f . eos . g

-- | end of string
eos :: PrinterParser TextsError [Text] r r
eos = PrinterParser
       (Parser $ \path pos -> case path of
                   []      -> [Right ((id, []), incMajor 1 pos)]
--                   [] -> mkParserError pos [EOI "input"]
                   (p:ps)
                       | Text.null p ->
                          [ Right ((id, ps), incMajor 1 pos) ]
                       | otherwise -> mkParserError pos [Message $ "path-segment not entirely consumed: " ++ (Text.unpack p)])
       (\a -> [((Text.empty :), a)])

-- | statisfy a 'Char' predicate
satisfy :: (Char -> Bool) -> PrinterParser TextsError [Text] r (Char :- r)
satisfy p = val
  (Parser $ \tok pos ->
       case tok of
         []     -> mkParserError pos [EOI "input"]
         (s:ss) ->
             case Text.uncons s of
               Nothing -> mkParserError pos [EOI "segment"]
               (Just (c, cs))
                   | p c ->
                       [Right ((c, cs : ss), incMinor 1 pos )]
                   | otherwise ->
                       mkParserError pos [SysUnExpect $ show c]
  )
  (\c -> [ \paths -> case paths of [] -> [Text.singleton c] ; (s:ss) -> ((Text.cons c s):ss) | p c ])


-- | satisfy a 'Text' predicate.
--
-- Note: must match the entire remainder of the 'Text' in this segment
satisfyStr :: (Text -> Bool) -> PrinterParser TextsError [Text] r (Text :- r)
satisfyStr p = val
  (Parser $ \tok pos ->
       case tok of
         []          -> mkParserError pos [EOI "input"]
         (s:ss)
             | Text.null s -> mkParserError pos [EOI "segment"]
             | p s ->
                 do [Right ((s, Text.empty:ss), incMajor 1 pos )]
             | otherwise ->
                 do mkParserError pos [SysUnExpect $ show s]
  )
  (\str -> [ \strings -> case strings of [] -> [str] ; (s:ss) -> ((str `Text.append` s):ss) | p str ])

-- | ascii digits @\'0\'..\'9\'@
digit :: PrinterParser TextsError [Text] r (Char :- r)
digit = satisfy isDigit <?> "a digit 0-9"

-- | matches alphabetic Unicode characters (lower-case, upper-case and title-case letters,
-- plus letters of caseless scripts and modifiers letters).  (Uses 'isAlpha')
alpha :: PrinterParser TextsError [Text] r (Char :- r)
alpha = satisfy isAlpha <?> "an alphabetic Unicode character"

-- | matches white-space characters in the Latin-1 range. (Uses 'isSpace')
space :: PrinterParser TextsError [Text] r (Char :- r)
space = satisfy isSpace <?> "a white-space character"

-- | any character
anyChar :: PrinterParser TextsError [Text] r (Char :- r)
anyChar = satisfy (const True)

-- | matches the specified character
char :: Char -> PrinterParser TextsError [Text] r (Char :- r)
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
readshow :: (Read a, Show a) => PrinterParser TextsError [Text] r (a :- r)
readshow =
    val readParser s
    where
      s a = [ \strings -> case strings of [] -> [Text.pack $ show a] ; (s:ss) -> (((Text.pack $ show a) `Text.append` s):ss) ]

readParser :: (Read a) => Parser TextsError [Text] a
readParser =
    Parser $ \tok pos ->
        case tok of
          []                  -> mkParserError pos [EOI "input"]
          (p:_) | Text.null p -> mkParserError pos [EOI "segment"]
          (p:ps) ->
            case reads (Text.unpack p) of
              [] -> mkParserError pos [SysUnExpect (Text.unpack p), Message $ "decoding using 'read' failed."]
              [(a,r)] ->
                  [Right ((a, (Text.pack r):ps), incMinor ((Text.length p) - (length r)) pos)]

readIntegral :: (Integral a) => Text -> a
readIntegral s =
    case (Text.signed Text.decimal) s of
      (Left e) -> error $ "readIntegral: " ++ e
      (Right (a, r))
          | Text.null r -> a
          | otherwise -> error $ "readIntegral: ambiguous parse. Left over data: " ++ Text.unpack r


-- | the empty string
rEmpty :: PrinterParser e [Text] r (Text :- r)
rEmpty = xpure (Text.empty :-) $
              \(xs :- t) ->
                  if Text.null xs
                     then (Just t)
                     else Nothing

-- | the first character of a 'Text'
rTextCons :: PrinterParser e tok (Char :- Text :- r) (Text :- r)
rTextCons =
    xpure (arg (arg (:-)) (Text.cons)) $
          \(xs :- t) ->
              do (a, as) <- Text.uncons xs
                 return (a :- as :- t)

-- | construct/parse some 'Text' by repeatedly apply a 'Char' 0 or more times parser
rText :: PrinterParser e [Text] r (Char :- r)
      -> PrinterParser e [Text] r (Text :- r)
rText r = manyr (rTextCons . duck1 r) . rEmpty

-- | construct/parse some 'Text' by repeatedly apply a 'Char' 1 or more times parser
rText1 :: PrinterParser e [Text] r (Char :- r)
      -> PrinterParser e [Text] r (Text :- r)
rText1 r = somer (rTextCons . duck1 r) . rEmpty


-- | a sequence of digits
digits :: PrinterParser TextsError [Text] r (Text :- r)
digits = rText digit

-- | an optional - character
--
-- Typically used with 'digits' to support signed numbers
--
-- > signed digits
signed :: PrinterParser TextsError [Text] a (Text :- r)
       -> PrinterParser TextsError [Text] a (Text :- r)
signed r = opt (rTextCons . char '-') . r

-- | matches an 'Integral' value
--
-- Note that the combinator @(rPair . integral . integral)@ is ill-defined because the parse canwell. not tell where it is supposed to split the sequence of digits to produced two ints.
integral :: (Integral a, Show a) => PrinterParser TextsError [Text] r (a :- r)
integral = xmaph readIntegral (Just . Text.pack . show)  (signed digits)

-- | matches an 'Int'
-- Note that the combinator @(rPair . int . int)@ is ill-defined because the parse canwell. not tell where it is supposed to split the sequence of digits to produced two ints.
int :: PrinterParser TextsError [Text] r (Int :- r)
int = integral

-- | matches an 'Integer'
--
-- Note that the combinator @(rPair . integer . integer)@ is ill-defined because the parse can not tell where it is supposed to split the sequence of digits to produced two ints.
integer :: PrinterParser TextsError [Text] r (Integer :- r)
integer = integral

-- | matches any 'Text'
--
-- the parser returns the remainder of the current Text segment, (but does not consume the 'end of segment'.
--
-- Note that the only combinator that should follow 'anyText' is
-- 'eos' or '</>'. Other combinators will lead to inconsistent
-- inversions.
--
-- For example, if we have:
--
-- > unparseTexts (rPair . anyText . anyText)  ("foo","bar")
--
-- That will unparse to @Just ["foobar"]@. But if we call
--
-- > parseTexts (rPair . anyText . anyText)  ["foobar"]
--
-- We will get @Right ("foobar","")@ instead of the original @Right ("foo","bar")@
anyText :: PrinterParser TextsError [Text] r (Text :- r)
anyText = val ps ss
    where
      ps = Parser $ \tok pos ->
           case tok of
             []     -> mkParserError pos [EOI "input", Expect "any string"]
--             ("":_) -> mkParserError pos [EOI "segment", Expect "any string"]
             (s:ss) -> [Right ((s, Text.empty:ss), incMinor (Text.length s) pos)]
      ss str = [\ss -> case ss of
                         []      -> [str]
                         (s:ss') -> ((str `Text.append` s) : ss')
               ]

-- | Predicate to test if we have parsed all the Texts.
-- Typically used as argument to 'parse1'
--
-- see also: 'parseTexts'
isComplete :: [Text] -> Bool
isComplete []   = True
isComplete [t]  = Text.null t


-- | run the parser
--
-- Returns the first complete parse or a parse error.
--
-- > parseTexts (rUnit . lit "foo") ["foo"]
parseTexts :: PrinterParser TextsError [Text] () (r :- ())
             -> [Text]
             -> Either TextsError r
parseTexts pp strs =
    either (Left . condenseErrors) Right $ parse1 isComplete pp strs

-- | run the printer
--
-- > unparseTexts (rUnit . lit "foo") ()
unparseTexts :: PrinterParser e [Text] () (r :- ()) -> r -> Maybe [Text]
unparseTexts pp r = unparse1 [] pp r
