module Rhino.Parser
  ( moduleParser
  , fileParser
  ) where

import Rhino.Prelude

import Data.Char (isSpace, toLower)
import qualified Data.Text as Text

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Text.Megaparsec hiding (Label)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Rhino.AST



--------------------------------------------------------------------------------
-- * Basics
--------------------------------------------------------------------------------

data Keyword
  = PUBLIC
  | PRIVATE
  | IMPORT
  | INPUT
  | TYPE
  | LABEL
  | DEF
  | REDEF
  | END
  | TRUE
  | FALSE
  deriving (Eq, Show, Enum, Bounded)

-- The lexer works slightly differently from the one in the Megaparsec tutorial
-- in that it consumes whitespace before each lexeme, but not after. This allows
-- us to easily determine whether a lexeme was preceded by a newline. We want to
-- have something like Ruby's syntax, which isn't indentation-sensitive, but
-- sometimes newline-sensitive.
--
-- A problem with consuming space before lexemes is that we have to backtrack to
-- before the whitespace when parsing fails. That's the reason for the use of
-- `try` in `lexemeLO`.

data LineOrder
  = Leading   -- ^ First lexeme on line
  | Following -- ^ Not first lexeme on line
  deriving (Eq, Show)

type Parser = Parsec Void Text

-- | Consume whitespace and comments
--
-- The result tells whether the space contained a newline.
spaceConsumer :: Parser Bool
spaceConsumer = any (`elem` ['\n', ';']) <$> some spaceLike
  where
    spaceLike =
      satisfy (\c -> isSpace c || c == ';')
        <|>
      (L.skipLineComment "#" *> return ' ')
  -- Note: Semicolon treated like newline

lexemeLO :: Parser a -> Parser (a, LineOrder)
lexemeLO p = try $ do
    -- `try` is used to avoid consuming space if the lexeme fails
  startOfFile <- (0 ==) <$> getOffset
  hasNewline <- fromMaybe False <$> optional spaceConsumer
  (, if startOfFile || hasNewline then Leading else Following) <$> p

-- | Parse a lexeme that must be the first on its line
leadLexeme :: Parser a -> Parser a
leadLexeme p = do
  st <- getParserState
  (a, lo) <- lexemeLO p
  unless (lo == Leading) $ do
    setParserState st
    fail "expected newline"
  return a

-- | Parse a lexeme that must not be the first on its line
followLexeme :: Parser a -> Parser a
followLexeme p = do
  st <- getParserState
  (a, lo) <- lexemeLO p
  unless (lo == Following) $ do
    setParserState st
    fail "unexpected newline"
  return a

lexeme :: Parser a -> Parser a
lexeme = fmap fst . lexemeLO

-- | Parse a symbol that must be the first lexeme on its line
leadSymbol :: Text -> Parser Text
leadSymbol = leadLexeme . string

symbol :: Text -> Parser Text
symbol = lexeme . string

wordChar :: Parser Char
wordChar = alphaNumChar <|> char '_'

-- | Parse a keyword that must be the first lexeme on its line
leadKeyword :: Keyword -> Parser ()
leadKeyword kw = void $ try $ leadSymbol kw' <* notFollowedBy wordChar
  -- `try` is used to back track if the symbol is followed by a word character
  where
    kw' = fromString $ map toLower $ show kw

keyword :: Keyword -> Parser ()
keyword kw = void $ try $ symbol kw' <* notFollowedBy wordChar
  where
    kw' = fromString $ map toLower $ show kw

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

tuple :: Parser a -> Parser [a]
tuple p = parens $ sepBy p (symbol ",")



--------------------------------------------------------------------------------
-- * Parser
--------------------------------------------------------------------------------

keywords :: [Text]
keywords =
  map (fromString . map toLower . show) [minBound .. maxBound :: Keyword]

identifier :: Parser Identifier
identifier = try $ do
  ident <- lexeme (fromString <$> ((:) <$> letterChar <*> many wordChar))
  when (ident `elem` keywords) $ fail "unexpected keyword"
  return $ coerce ident

boolean :: Parser Bool
boolean = (keyword TRUE *> pure True) <|> (keyword FALSE *> pure False)

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed (return ()) integer

signedFloat :: Parser Double
signedFloat = L.signed (return ()) float

stringLiteral :: Parser String
stringLiteral = lexeme (char '\"' *> manyTill L.charLiteral (char '\"'))

literal :: Parser Literal
literal = choice
  [ Boolean      <$> boolean
  , Integer      <$> signedInteger
  , Float        <$> signedFloat
  , String . toS <$> stringLiteral
  ]

term :: Parser Expression
term = choice
  [ parens expression
  , Literal <$> literal
  , do var <- identifier
       args <- optional $ tuple expression
       return $ maybe (Variable var) (FunCall var) args
  ]

expression :: Parser Expression
expression = makeExprParser term operatorTable

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [ InfixL (BinOp Times <$ symbol "*")
    , InfixL (BinOp Div   <$ symbol "/")
    ]
  , [ InfixL (BinOp Plus  <$ symbol "+")
    , InfixL (BinOp Minus <$ symbol "-")
    ]
  ]

-- | Parse a leading keyword preceeded by an optional scope qualifier
scopedKeyword :: Keyword -> Parser (Maybe Scope)
scopedKeyword kw = try $ choice
  [ leadKeyword PUBLIC  *> keyword kw *> return (Just Public)
  , leadKeyword PRIVATE *> keyword kw *> return (Just Private)
  , leadKeyword kw *> return Nothing
  ]

path :: Parser Text
path = followLexeme $ fromString <$> some (wordChar <|> char '/')

importDecl :: Parser Import
importDecl = do
  importScope <- fromMaybe Private <$> scopedKeyword IMPORT
  importLoc   <- getSourcePos
  importPath  <- coerce path
  return Import {..}

inputTypeDecl :: Parser Identifier
inputTypeDecl = leadKeyword TYPE *> symbol ":" *> identifier

inputLabelDecl :: Parser Text
inputLabelDecl = leadKeyword LABEL *> symbol ":" *> lbl
  where
    lbl = Text.strip . fromString <$> manyTill L.charLiteral (lookAhead $ char '\n')

input :: Parser Input
input = do
  inputScope <- fromMaybe Public <$> scopedKeyword INPUT
  inputLoc <- getSourcePos
  inputName <- identifier
  tls <- many (fmap Left inputTypeDecl <|> fmap Right inputLabelDecl)
  inputType <- case [t | Left t <- tls] of
    []  -> return Nothing
    [t] -> return $ Just t
    _   -> fail "multiple type declarations"
  inputLabel <- case [l | Right l <- tls] of
    []  -> return Nothing
    [l] -> return $ Just l
    _   -> fail "multiple labels"
  keyword END
  return Input {..}

localDef :: Parser LocalDef
localDef = LocalDef <$> (identifier <* symbol "=") <*> expression

definition :: Parser Definition
definition = do
  (re, defScope) <- second (fromMaybe Public) <$>
    (   fmap (False, ) (scopedKeyword DEF)
    <|> fmap (True, )  (scopedKeyword REDEF)
    )
  Definition re
    <$> getSourcePos
    <*> pure defScope
    <*> identifier
    <*> args
    <*> many (try localDef)
    <*> expression
    <*  keyword END
  where
    args = maybe [] id <$> (optional $ tuple identifier)

moduleParser :: Parser Module
moduleParser =
  Module
    <$> getSourcePos
    <*> many importDecl
    <*> many (fmap InputDecl input <|> fmap DefDecl definition)

fileParser :: Parser Module
fileParser = moduleParser <* optional spaceConsumer <* eof
