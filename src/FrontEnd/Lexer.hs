module FrontEnd.Lexer(lexString) where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Language
import qualified Text.Parsec.Token as Tok

import FrontEnd.Token

lexString :: String -> String -> Either String [Token]
lexString sourceFileName str = case parse (sepBy pTok spaces) sourceFileName str of
  Left err -> Left $ show err
  Right toks -> Right toks

languageDef =
  emptyDef { Tok.commentStart    = "/*",
             Tok.commentEnd      = "*/",
             Tok.commentLine     = "//",
             Tok.identStart      = letter,
             Tok.identLetter     = alphaNum,
             Tok.reservedNames   = [ "architecture", "operation", "LangC",
                                     "mat", "vec", "sca",
                                     "rm", "cm",
                                     "iarg", "oarg", "temp",
                                     "double", "single"],
             Tok.reservedOpNames = ["+", "-", "*", "/", "==", "<", ">", "<=", ">=", "||", "&&", "~"] }

lexer = Tok.makeTokenParser languageDef

pTok :: Parser Token
pTok = pVarOrRes

pVarOrRes = (try pIdentifier) <|> pResWord <|> pLit

pIdentifier = do
  pos <- getPosition
  first <- pVarName
  return $ ident first pos

pVarName = Tok.identifier lexer

pResWord = do
  pos <- getPosition
  resStr <- try (string "operation")
         <|> string "architecture"
         <|> string "LangC"
         <|> string "mat"
         <|> string "vec"
         <|> string "oarg"
         <|> string "iarg"
         <|> string "temp"
         <|> string "double"
         <|> (try (string "single"))
         <|> string "sca"
         <|> string "rm"
         <|> string "cm"
         <|> string "{"
         <|> string "}"
         <|> string "("
         <|> string ")"
         <|> string ","
         <|> string "="
         <|> string ";"
         <|> string "+"
         <|> string "-"
         <|> string ".*"
         <|> string "*"
         <|> string "'"
  return $ res resStr pos

pLit = do
  pos <- getPosition
  digs <- many1 digit
  return $ intLit (read digs) pos
