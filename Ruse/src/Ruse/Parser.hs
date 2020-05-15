module Ruse.Parser where

import Ruse.Syntax

import Control.Applicative (Alternative, liftA2)
import Control.Monad
import Control.Monad.Combinators
import Data.Char (isDigit, isAlpha, digitToInt)
import Data.Functor
import Data.List (elemIndex, stripPrefix, nub)
import qualified Data.Map.Strict as Map

-- Parsing is a common task for converting unstructured data (strings, bit
-- vectors, etc.) into structured data (programs, syntax trees, packets, etc.).
-- In this homework, we will develop a miniature Haskell parsing library from
-- scratch, along with a set of general parser combinators for building up more
-- complex parsers out of simpler parsers. Finally, we will use the parser
-- library to build a simple calculator: parse input expressions, and return the
-- numeric answer. The library will give you hands-on experience with some of
-- the Haskell features we have seen in class: typeclasses and monads.
--
-- We will provide the setup code for parsing, along with useful utilities. Your
-- task is to define the key logic and the combinators.

-- To start with, we define the type of Parsers. These are functions that take
-- an input state (PState) containing the input string, and try to extract a value of
-- type a from a string. If a value of type a is successfully extracted, it is
-- returned along with the resulting state in the parse result (PResult a).
newtype Parser a = MkParser { runParser :: PState -> (PState, PResult a) }

-- The parser state can contain a lot of auxiliary information. To keep it
-- simple, our parser state will include just two pieces of information: the
-- remaining string to be parsed, and an integer offset representing how many
-- characters we have parsed so far.
data PState = MkPState { toParse  :: String
                       , stOffset :: !Int
                       } deriving Show
-- ! is called a "strictness" annotation: it requires the offset to be evaluated
-- to an integer before it is wrapped up into a PState, rather than lazily. We
-- won't talk about these issues in this class, but thinking about performance
-- in Haskell requires deciding which parts should be strict, and which parts
-- should be lazy. 

-- Turning to the result, a parser's result is either a successful parsed value
-- of type a, or a parse error of some kind.
data PResult a = ParseOK a | ParseError PError deriving Show

-- To provide the user with useful error messages, sophisticated parsers keep
-- track of a lot of side information. We will include three things: an integer
-- offset indicating where the parse error occurred, possibly information about
-- what item was found, and a list of items that were expected.
data PError = MkPError { erOffset :: !Int
                       , found    :: Maybe ErrorChunk
                       , expected :: [ErrorChunk]
                       } deriving Eq

-- Each ErrorChunk consists of either just a string, or a special token
-- indicating the end of the string (for when we expect the string to end but it
-- does not, or when the string unexpectedly ends in the middle of parsing).
data ErrorChunk = Chunk String | EndOfInput deriving Eq

-- To print parser results and errors in a human-readable form, we define the
-- following Show instances.
instance Show PError where
  show e = 
    let loc = "Error at position " ++ show (erOffset e) ++ "\n"
        got = maybe "" (\ec -> "Found: " ++ show ec ++ "\n") (found e)
        ex = if null (expected e)
             then ""
             else "Expected: " ++ show (expected e) ++ "\n"
    in loc ++ got ++ ex

instance Show ErrorChunk where
  show (Chunk str)  = str
  show EndOfInput = "EOF"

-- For our parsing library, we want to combine simpler parsers to build more
-- complex parsers. The first step is to define how to combine parsers together.
-- Experience has shown certain that there are certain common patterns for
-- combining programs (not just parsers) together.
--
-- The first way to combine two parsers is to run one parser, then run the
-- second parser on the rest of the string. This combination pattern can be
-- summed up by defining operations to make Parser into a Monad.
instance Monad Parser where
  return = pReturn
  (>>=)  = pBind

-- Define the return operation, which gives a parser that always yields the
-- given value of type a without changing the parse state.
pReturn :: a -> Parser a
-- pReturn x = undefined
pReturn x = MkParser (\pState -> (pState, ParseOK x))

-- Define the bind operation for Parsers. This should run the first parser, and
-- look at the result. If the result is an error, the second parser should be
-- ignored. Otherwise, use the first result to choose which parser to run next.
pBind :: Parser a -> (a -> Parser b) -> Parser b
-- pBind p f = undefined
pBind p f = MkParser $ \pState ->
    case runParser p pState of
    (pState', ParseOK val) -> runParser (f val) pState'
    (pState', ParseError err) -> (pState', ParseError err)

-- The second way to combine two parsers is to try the first parser, or the
-- second parser. This pattern is modeled by the Alternative typeclass.
instance Alternative Parser where
  empty = pZero
  (<|>) = pPlus

-- The Alternative type class has an "empty" operation. This operation
-- should satisfy the following laws:
--
-- empty <|> p === p <|> empty === p
--
-- For parsers, empty is the parser that fails without changing the state.
pZero :: Parser a
pZero = MkParser $ \st -> (st, ParseError $ MkPError (stOffset st) Nothing [])

-- A variant to report an error: found a string, expecting list of strings.
pFail :: String -> [String] -> Parser a
pFail fnd ext = MkParser $ \st -> (st, ParseError $ MkPError (stOffset st) (Just $ Chunk fnd) (map Chunk ext))

-- Define the choice operator (<|>) on two parsers. The combined parser should
-- try the first parser, and if it fails, try the second parser. If the first
-- parser succeeds, the second parser should not be run. If both parsers fail,
-- then use the provided function mergeErrors to combine the two resulting
-- errors and states. (You will be using this operation a lot.)
pPlus :: Parser a -> Parser a -> Parser a
-- pPlus p1 p2 = undefined
pPlus p1 p2 = MkParser $ \pState ->
    case runParser p1 pState of
    (pState', ParseOK val) -> (pState', ParseOK val)
    (pState', ParseError err) -> 
      case runParser p2 pState of 
        (pState'', ParseOK val') -> (pState'', ParseOK val')
        (pState'', ParseError err') -> 
          case mergeErrors (pState', err) (pState'', err') of 
            (finalState, finalErr) -> (finalState, ParseError finalErr)

mergeErrors :: (PState, PError) -> (PState, PError) -> (PState, PError)
mergeErrors (st1, e1) (st2, e2)
  | erOffset e1 > erOffset e2 = (st1, e1)
  | erOffset e1 < erOffset e2 = (st2, e2)
  | otherwise = (st1, MkPError off fnd ex)
  where
    off = erOffset e1
    fnd = case (found e1, found e2) of
            (Nothing, Nothing) -> Nothing
            (Just x , Nothing) -> Just x
            (Nothing, Just y ) -> Just y
            (Just x , Just _ ) -> Just x
    ex = nub $ expected e1 ++ expected e2

-- The MonadPlus typeclass describes things that are both Monad and Alternative.
instance MonadPlus Parser where
  mzero = pZero
  mplus = (<|>)

-- We now define some other useful operations on parsers. Keep an eye out later
-- for opportunities to use them---they can help you define parsers more
-- concisely.
instance Functor Parser where
  fmap = pMap

-- The fmap operation takes a function from a -> b, and transforms a Parser
-- producing a's into a Parser producing b's. The fmap operation can also be
-- written f <$> p.
pMap :: (a -> b) -> Parser a -> Parser b
pMap f p = do { x <- p ; return $ f x }

-- The operations making Parser into an Applicative instance are a bit trickier
-- to read. The last two come in handy quite often. The first, p1 *> p2, runs
-- p1, forgets the parsed value, and then runs p2. The second, p1 <* p2, runs
-- p1, remembers the parsed value, and forgets the parsed value from running p2.
-- Both parsers are run, but you can think of the arrow as pointing to the
-- parser whose result is returned as the final parsed value.
instance Applicative Parser where
  pure  = pReturn
  (<*>) = pApp
  p1 *> p2 = do { p1 ; p2 }
  p1 <* p2 = do { x <- p1 ; p2 ; return x }

pApp :: Parser (a -> b) -> Parser a -> Parser b
pApp pf p = do f <- pf
               f <$> p

-- Now, we define some functions to run our parsers given an input string. These
-- functions will be useful for testing your parser in ghci. The parse function
-- runs a parser on an input string, returning the final result. The final
-- state is discarded.
parse :: Parser a -> String -> PResult a
parse p input = snd . runParser p $ MkPState input 0

-- The parseTest operation does the same thing, except it returns a Maybe: this
-- holds the parsed value if the parse succeeded, or Nothing if was an error.
parseTest :: Parser a -> String -> Maybe a
parseTest p input =
  case parse p input of
    ParseOK val  -> Just val
    ParseError _ -> Nothing

-- Now, we will build up some useful parsers.
--
-- The base parser will parse a single character satisfying a predicate.
-- Concretely,
--
-- `token predicate expected`
--
-- should parse a single character c from the front of the string if (predicate
-- c) is true. Remember to update the state of the parser: the offset in PState
-- should be incremented by 1, and the remaining string to parse should be
-- updated.
--
-- If (predicate c) is not true, the parser should produce an ParseError at the
-- current offset indicating that it found letter c and it was expecting one of
-- the items in expected. If the string is empty, the parser should produce an
-- error indicating that it found the EndOfInput.
--
-- You should feel free to use the parser combinators described here:
--
-- https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators.html
token :: (Char -> Bool) -> [ErrorChunk] -> Parser Char
-- token predicate ex = undefined
token predicate expected = MkParser $ \pState ->
  let (MkPState toParse stOffset) = pState
  in case toParse of
          [] -> (pState, ParseError (MkPError stOffset (Just EndOfInput) expected))
          (c:cs) ->
            if predicate c 
            then (MkPState cs (stOffset + 1), ParseOK c)
            else (pState, ParseError (MkPError stOffset (Just (Chunk [c])) expected))

-- Use token to define single, which parses exactly the given character from the
-- front of the string. Just like token, it's fine if the input string contains
-- more characters.
single :: Char -> Parser Char
-- single c = undefined
single c = token (== c) [Chunk [c]]

-- GHCI TEST: parseTest (single 'a') "a" === Just 'a'
-- GHCI TEST: parseTest (single 'a') "ab" === Just 'a'
-- GHCI TEST: parseTest (single 'a') "c" === Nothing

-- GHCI TEST: parseTest ((single 'a' >> single 'b') <|> (single 'a' >> single 'c')) "ac" === Just 'c'
-- GHCI TEST: parseTest (single 'a' <|> (single 'a' >> single 'c')) "ac" === Just 'a'
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single c)) "a" === Nothing
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single c)) "aa" === Just 'a'
-- GHCI TEST: parseTest (single 'a' >>= (\c -> single 'b' >> single c)) "aba" === Just 'a'

-- eof succeeds exactly when the remaining string is empty, otherwise it fails.
eof :: Parser ()
-- eof = undefined
eof = MkParser $ \pState ->
  let (MkPState toParse stOffset) = pState
  in case toParse of
          [] -> (pState, ParseOK ())
          (c:cs) -> (pState, ParseError (MkPError stOffset (Just (Chunk [c])) [EndOfInput]))

-- GHCI TEST: parseTest eof "" === Just ()
-- GHCI TEST: parseTest eof "nonempty" === Nothing

-- chunk is like token, except it parses a target string instead of a character.
chunk :: String -> Parser String
chunk cs = MkParser $ \st ->
  case stripPrefix cs (toParse st) of
    Nothing   -> (st, ParseError $ MkPError (stOffset st) Nothing [Chunk cs])
    Just rest -> (MkPState rest (stOffset st + length cs), ParseOK cs)

-- satisfy parses one character satisfying the predicate.
--
-- (Hint: try using token. You can use an empty list of expected characters.)
satisfy :: (Char -> Bool) -> Parser Char
-- satisfy predicate = undefined
satisfy predicate = token predicate []

-- GHCI TEST: parseTest (satisfy (`elem` "aeiou")) "a" === Just 'a'
-- GHCI TEST: parseTest (satisfy (`elem` "aeiou")) "z" === Nothing

-- oneOf parses any one in a list of characters
oneOf :: String -> Parser Char
-- oneOf cs = undefined
oneOf cs = satisfy (`elem` cs)

-- A few special parsers will be useful for parsing our language. First, we
-- parsers for space characters. space parses a single space character.
space :: Parser Char
space = oneOf [' ', '\t', '\r', '\n']

-- Define a parser optSpaces that parses zero or more spaces.
--
-- (Hint: try the `skipMany` combinator.)
optSpaces :: Parser ()
-- optSpaces = undefined
optSpaces = skipMany space

-- Define a parser spaces that parses one or more spaces.
--
-- (Hint: try the `skipSome` combinator.)
spaces :: Parser ()
-- spaces = undefined
spaces = skipSome space

-- Define a parser symbol that parses a given string, followed by zero or more
-- spaces. The target string should be returned, and the spaces should be
-- discarded. This combinator is useful when the target string is a symbol, and
-- no spaces are needed afterwards.
symbol :: String -> Parser String
-- symbol sym = undefined
symbol sym = optSpaces *> chunk sym <* optSpaces

-- GHCI TEST: parseTest (symbol "bobcat") "bobcat  " === Just "bobcat"
-- GHCI TEST: parseTest (symbol "bobcat") "bobcat" === Just "bobcat"
-- GHCI TEST: parseTest (symbol "bobcat") "bob" === Nothing

-- Define a parser keyword that parses a given string, followed by one or more
-- spaces. The target string should be returned, the spaces should be discarded.
-- This combinator is useful when the target string is a keyword of some kind,
-- where there must be at least one space afterwards to separate it from the
-- next character.
keyword :: String -> Parser String
-- keyword kw = undefined
keyword kw = optSpaces *> chunk kw <* spaces

-- GHCI TEST: parseTest (keyword "bobcat") "bobcat  " === Just "bobcat"
-- GHCI TEST: parseTest (keyword "bobcat") "bobcat" === Nothing
-- GHCI TEST: parseTest (keyword "bobcat") "bob" === Nothing

-- Define a parser to parse "#t" or "#f" into a Bool.
boolean :: Parser Bool
-- boolean = undefined
boolean = (symbol "#t" >> return True) <|> (symbol "#f" >> return False)

-- Now, we'll build parsers for digits into Ints
digit :: Parser Int
digit = digitToInt <$> satisfy isDigit

digitsToInt :: [Int] -> Int
digitsToInt = foldl (\cur new -> 10 * cur + new) 0
 
-- Using these helper functions, define a parser to parse a nonempty string of
-- numbers (possibly starting with "-") into an integer.
--
-- (Hint: a number is some digits, or a "-" followed by some digits.)
number :: Parser Int
-- number = undefined
number =  digitsToInt <$> some digit 
        <|>
          do symbol "-"
             r <- digitsToInt <$> some digit 
             return $ -r 

-- GHCI TEST: parseTest number "12345" === Just 12345
-- GHCI TEST: parseTest number "-42" === Just (-42)
-- GHCI TEST: parseTest number "bob" === Nothing

-- The string parser parses a string surrounded by double quote marks.
--
-- (Hint: try the `between` combinator.)
string :: Parser String
-- string = undefined
string = between (symbol "\"") (symbol "\"") (many $ token (`notElem` "\"") [])

--
-- Parsing Ruse
--
-- You'll now use your parser to parse a toy language, called Ruse.
--
--

--
--
-- Reserved keywords: these can't be variable names
--
--
reserved :: Context
reserved = [ "if"
           , "cond"
           , "and"
           , "or"
           , "not"
           , "eq?"
           , "nil?"
           , "list"
           , "cons"
           , "car"
           , "cdr"
           , "lambda"
           , "define"
           , "else"
           ]

-- Ruse variable names must satisfy the following conditions:
--
-- 1) They must start with an alphabetical character
-- 2) The rest of the letters can be any combination of non-space symbols,
-- excluding the reserved symbols:
--
-- '(', ')', '[', ']', '{', '}', '"', '\'', ',', '`', ';', '#', '|'
--
-- 3) They can't be a reserved name.
--
-- Build a parser for Ruse variable names (identifiers).
ident :: Parser String
ident = do
  n <- token isAlpha []
  ns <- many $ token (`notElem` "()[]{}\",'`;#| \t\r\n") []
  let name = n:ns in
    if name `elem` reserved
      then pFail name ["Non-reserved variable name"]
      else return name

-- We'll use two structures to track variables as we parse.
--
-- `Globals` maps variable names to expressions. These represent globally
-- defined things. This map is *not* changed as we parse an expression.
type Globals = Map.Map String RExpr

-- `Context` is a list of variable names. The inner-most variable (the most
-- recently-seen one) is stored first; the outer-most variable (the earliest
-- one) is stored last. This list *is* changed when the parser tries to parse
-- the body of a lambda expression `Lam e` or a recursive definition `Rec e`.
type Context = [String]

--
--
-- Given a Globals map, we build a parser for RExpr.
--
--
parseRExpr :: Globals -> Parser RExpr
parseRExpr gctx = parseRExpr' gctx []

--
--
-- Main parsing routine
--
-- We will only be testing parseRExpr, not the functions below, so you should
-- feel free to change things/add new functions as you like. One way to
-- implement the parser is to define a parser for each case of the AST---we've
-- done a few of the cases for you. However, you may also notice that many of
-- these parsers look the same, and it's possible to abstract even further
-- (hint: unary AST nodes, binary AST nodes, ternary AST nodes, ...).
--
-- For almost all of the parsers, you will be passing the global and local
-- contexts into recursive calls without making any changes. But see parseLam
-- and parseRec/parseDef below for the two cases where you will need to modify
-- the local context.
--
--
parseRExpr' :: Globals -> Context -> Parser RExpr
parseRExpr' gctx ctx =  parseNumC
                    <|> parseBoolC
                    <|> parseStrC
                    <|> parsePlus gctx ctx
                    <|> parseSubt gctx ctx
                    <|> parseMult gctx ctx
                    <|> parseIfte gctx ctx
                    <|> parseAnd gctx ctx
                    <|> parseOr gctx ctx
                    <|> parseNot gctx ctx
                    <|> parseIsEq gctx ctx
                    <|> parseIsLt gctx ctx
                    <|> parseIsGt gctx ctx
                    <|> parseIsNil gctx ctx
                    <|> parseList gctx ctx
                    <|> parseEList gctx ctx
                    <|> parseCons gctx ctx
                    <|> parseCar gctx ctx
                    <|> parseCdr gctx ctx
                    <|> parseVar gctx ctx
                    <|> parseLam gctx ctx
                    <|> parseApp gctx ctx
                    <|> parseRec gctx ctx
                    <|> parseCond gctx ctx

parseNumC :: Parser RExpr
parseNumC = NumC <$> number
             
parseBoolC :: Parser RExpr
parseBoolC = BoolC <$> boolean

parseStrC :: Parser RExpr
parseStrC = StrC <$> string

parsePlus :: Globals -> Context -> Parser RExpr
parsePlus gctx ctx = do symbol "("
                        symbol "+"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- Plus r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseSubt :: Globals -> Context -> Parser RExpr
parseSubt gctx ctx = do symbol "("
                        symbol "-"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- Subt r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseMult :: Globals -> Context -> Parser RExpr
parseMult gctx ctx = do symbol "("
                        symbol "*"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- Mult r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseIfte :: Globals -> Context -> Parser RExpr
parseIfte gctx ctx = do symbol "("
                        keyword "if"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- parseRExpr' gctx ctx <* optSpaces
                        r3 <- Ifte r1 r2 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r3
                        
parseAnd:: Globals -> Context -> Parser RExpr
parseAnd gctx ctx = do symbol "("
                       keyword "and"
                       r1 <- parseRExpr' gctx ctx <* optSpaces
                       r2 <- And r1 <$> parseRExpr' gctx ctx <* optSpaces
                       symbol ")"
                       return r2

parseOr:: Globals -> Context -> Parser RExpr
parseOr gctx ctx = do symbol "("
                      keyword "or"
                      r1 <- parseRExpr' gctx ctx <* optSpaces
                      r2 <- Or r1 <$> parseRExpr' gctx ctx <* optSpaces
                      symbol ")"
                      return r2

parseNot:: Globals -> Context -> Parser RExpr
parseNot gctx ctx = do symbol "("
                       keyword "not"
                       r1 <- Not <$> parseRExpr' gctx ctx <* optSpaces
                       symbol ")"
                       return r1

parseIsEq :: Globals -> Context -> Parser RExpr
parseIsEq gctx ctx = do symbol "("
                        keyword "eq?"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- IsEq r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseIsLt :: Globals -> Context -> Parser RExpr
parseIsLt gctx ctx = do symbol "("
                        keyword "<"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- IsLt r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseIsGt :: Globals -> Context -> Parser RExpr
parseIsGt gctx ctx = do symbol "("
                        keyword ">"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- IsGt r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseList :: Globals -> Context -> Parser RExpr
parseList gctx ctx = do symbol "("
                        symbol "list"
                        r1 <- many (parseRExpr' gctx ctx <* optSpaces)
                        symbol ")"
                        return $ List r1

parseEList :: Globals -> Context -> Parser RExpr
parseEList gctx ctx = do symbol "("
                         symbol ")"
                         return $ List []

parseIsNil :: Globals -> Context -> Parser RExpr
parseIsNil gctx ctx = do symbol "("
                         keyword "nil?"
                         r1 <- parseRExpr' gctx ctx <* optSpaces
                         symbol ")"
                         return $ IsNil r1

parseCons :: Globals -> Context -> Parser RExpr
parseCons gctx ctx = do symbol "("
                        keyword "cons"
                        r1 <- parseRExpr' gctx ctx <* optSpaces
                        r2 <- Cons r1 <$> parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        return r2

parseCar :: Globals -> Context -> Parser RExpr
parseCar gctx ctx = do symbol "("
                       keyword "car"
                       r1 <- Car <$> parseRExpr' gctx ctx <* optSpaces
                       symbol ")"
                       return r1

parseCdr :: Globals -> Context -> Parser RExpr
parseCdr gctx ctx = do symbol "("
                       keyword "cdr"
                       r1 <- Cdr <$> parseRExpr' gctx ctx <* optSpaces
                       symbol ")"
                       return r1

-- Parse a variable, using the global context to unfold global definitions and
-- using the local context to look up local variable names. You probably don't
-- want to modify this function.
parseVar :: Globals -> Context -> Parser RExpr
parseVar gctx ctx = do
  name <- ident
  case elemIndex name ctx of
    Nothing -> case Map.lookup name gctx of
      Nothing -> pFail name ctx
      Just re -> pure re
    Just ix -> pure (Var $ ix + 1)

-- Parse a Lambda expression
--
-- Lambda expressions look like the following:
--
-- (lambda var-name (+ var-name 1))
--
-- To parse this expression, you should parse the "(lambda", one-or-more spaces,
-- then parse the variable name "var-name" with `ident`. Then, recursively parse
-- the body expression after adding the variable name to the *front* of the
-- local context. (You should not change the global context in this file.)
-- Finally, you will parse zero-or-more spaces and then ")".

parseLam :: Globals -> Context -> Parser RExpr
parseLam gctx ctx = do symbol "("
                       keyword "lambda"
                       r1 <- ident <* optSpaces
                       r2 <- parseRExpr' gctx (r1:ctx) <* optSpaces
                       symbol ")"
                       return $ Lam r2

parseApp :: Globals -> Context -> Parser RExpr
parseApp gctx ctx = do symbol "("
                       r1 <- parseRExpr' gctx ctx <* optSpaces
                       r2 <- App r1 <$> parseRExpr' gctx ctx <* optSpaces
                       symbol ")"
                       return r2       

parseRec :: Globals -> Context -> Parser RExpr
parseRec gctx ctx = snd <$> parseDef gctx ctx

-- Parse a (possibly) recursive definition
--
-- Recursive definitions look like the following:
--
-- (define my-def-name (cons 1 my-def-name))
--
-- This definition represents an infinite list of 1's. (Note: since Ruse is not
-- a lazy language like Haskell, you should *not* try to evaluate this program.)
--
-- To parse this expression, you should parse the "(define", one-or-more spaces,
-- then parse the definition name "my-def-name" with `ident`. Then, recursively
-- parse the body expression after adding the definition name to the *front* of
-- the local context. (You should not change the global context in this file.)
-- Finally, you will parse zero-or-more spaces and then ")". Return a pair of
-- the name of the definition (my-def-name) and the parsed expression.
--
-- For instance, if the input string is:
--
-- (define foo 5)
--
-- your parser should produce ("foo", Rec (NumC 5)).

parseDef :: Globals -> Context -> Parser (String, RExpr)
parseDef gctx ctx = do symbol "("
                       keyword "define"
                       r1 <- ident <* optSpaces
                       r2 <- parseRExpr' gctx (r1:ctx) <* optSpaces
                       symbol ")"
                       return (r1, Rec r2)

-- Optional Cond
parseCond :: Globals -> Context -> Parser RExpr
parseCond gctx ctx = do symbol "("
                        keyword "cond"
                        r0 <-   do symbol "("
                                   r1 <- many ( do symbol "("
                                                   guard <- parseRExpr' gctx ctx <* optSpaces
                                                   val <- parseRExpr' gctx ctx <* optSpaces
                                                   symbol ")"
                                                   return (guard, val)
                                              )
                                   symbol ")" 
                                   return r1
                              <|> 
                                do symbol "("
                                   symbol ")" 
                                   return []
                        symbol "("
                        keyword "else"
                        r2 <- parseRExpr' gctx ctx <* optSpaces
                        symbol ")"
                        symbol ")"
                        return $ Cond r0 r2