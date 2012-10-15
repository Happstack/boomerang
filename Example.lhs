Boomerang is a DSL for creating parsers and pretty-printers using a
single specification. Instead of writing a parser, and then writing a
separate pretty-printer, both are created at once. This saves time,
and ensures that the parser and pretty-printer are inverses and stay
in-sync with each other.

Boomerang is a generalized derivative of the Zwaluw library created by
Sjoerd Visscher and Martijn van Steenbergen:

<http://hackage.haskell.org/package/Zwaluw>

Boomerang is similar in purpose, but different in implementation to:

<http://hackage.haskell.org/package/invertible-syntax>

Here is a simple example. First we enable three language extensions:

@ {\-\# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings \#-\} @

In the imports, note that we hide @((.), id)@ from 'Prelude' and use
@((.), id)@ from "Control.Category" instead.

> {-# LANGUAGE TemplateHaskell, TypeOperators, OverloadedStrings #-}
> module Main where
>
> import Prelude hiding ((.), id)
> import Control.Category ((.), id)
> import Control.Monad (forever)
> import Text.Boomerang
> import Text.Boomerang.String
> import Text.Boomerang.TH
> import System.IO (hFlush, stdout)

Next we define a type that we want to be able to pretty-print and define parsers for:

> data Foo
>     = Bar
>     | Baz Int Char
>       deriving (Eq, Show)

Then we generate some combinators for the type:

> $(makePrinterParsers ''Foo)

The combinators will be named after the constructors, but with an r prefixed to them. In this case, @rBar@ and @rBaz@.

Now we can define a grammar:

> foo :: StringPrinterParser () (Foo :- ())
> foo = 
>     (  rBar 
>     <> rBaz . "baz-" . int . "-" . alpha
>     )

@.@ is used to compose parsers together. '<>' is used for choice.

Now we can use @foo@ as a printer or a parser.

Here is an example of a successful parse:

> test1 = parseString foo "baz-2-c"

@
*Main> test1
Right (Baz 2 'c')
@

And another example:

> test2 = parseString foo ""

@
*Main> test2
Right Bar
@

Here is an example of a parse error:

> test3 = parseString foo "baz-2-3"

@
*Main> test3
Left parse error at (0, 6): unexpected '3'; expecting an alphabetic Unicode character
@

we can also use @foo@ to pretty-print a value:

> test4 = unparseString foo (Baz 1 'z')

@
*Main> test4
Just "baz-1-z"
@

Here is a little app that allows you to interactively test @foo@.

> testInvert :: String -> IO ()
> testInvert str =
>     case parseString foo str of
>       (Left e) -> print e
>       (Right f') ->
>           do putStrLn $ "Parsed: " ++ show f'
>              case unparseString foo f' of
>                Nothing  -> putStrLn "unparseString failed to produce a value."
>                (Just s) -> putStrLn $ "Pretty: " ++ s

> main = forever $ 
>     do putStr "Enter a string to parse: "
>        hFlush stdout
>        l <- getLine
>        testInvert l
