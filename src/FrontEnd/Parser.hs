module FrontEnd.Parser({-parseOperation,
                       parseStatement,
                       parseFormalParam) where-}
                       parseFormalParam) where

import Text.Parsec.Expr
import Text.Parsec.Prim
import Text.ParserCombinators.Parsec

import FrontEnd.Token
import IndexExpression
import Matrix
import MatrixOperation
import Type

parser :: Parsec [Token] () a -> String -> [Token] -> Either String a
parser p srcName toks = case parse p srcName toks of
  Left err -> Left $ show err
  Right res -> Right $ res

parseOperation :: String -> [Token] -> Either String [[MatrixStmt]]
parseOperation sourceFileName toks = case parse (many pOperation) sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp

{-parseStatement :: String -> [Token] -> Either String MatrixStmt
parseStatement sourceFileName toks = case parse pMatStatement sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp-}

parseFormalParam srcName toks = parser pFormalParam srcName toks

pFormalParam = do
  scp <- pScope
  (nr, nc, rs, cs) <- pLayout
  tp <- pEntryType
  (name, pos) <- pIdent
  return $ (name, matrix name nr nc rs cs $ properties scp tp memory)

pEntryType = do
  scopeStr <- pResWithNameTok "double" <|> pResWithNameTok "single"
  case resName scopeStr of
    "double" -> return double
    "single" -> return single

pLayout = pScalarL <|> pVectorL <|> pMatrixL

pScalarL = do
  s <- pResWithNameTok "sca"
  return (iConst 1, iConst 1, iConst 1, iConst 1)

pVectorL = error "pVectorL"

pMatrixL = error "pMatrixL"

pScope = do
  scopeStr <- pResWithNameTok "iarg" <|> pResWithNameTok "oarg" <|> pResWithNameTok "temp"
  case resName scopeStr of
    "iarg" -> return arg
    "oarg" -> return arg
    "temp" -> return local


pOperation = error "pOperation" {-do
  position <- getPosition
  pResWithNameTok "operation"
  (name, pos) <-pIdent
  pResWithNameTok "("
  args <- pArgList
  pResWithNameTok ")"
  pResWithNameTok "{"
  ops <- pMatStatements
  pResWithNameTok "}"
  return $ matrixOperation name args ops position-}

{-
pArgList = sepBy pFormalParam (pResWithNameTok ",")


pReadMod = (pResWithNameTok "output") <|> (pResWithNameTok "r") <|> (pResWithNameTok "rw")

pEntryType = do
  outN <- pResWithNameTok "double" <|> pResWithNameTok "float"
  case resName outN of
    "double" -> return double
    "float" -> return single

pLayout = do
  nr <- pDim
  nc <- pDim
  rs <- pDim
  cs <- pDim
  return $ layoutForMat nr nc rs cs

layoutForMat nr nc rs cs name = error "layoutForMat" {-
  layout (lr nr "_nrows") (lr nc "_ncols") (lr rs "_rs") (lr cs "_cs")
  where
    lr i suffix = if i == iVar "gen" then (iVar (name ++ suffix)) else i-}

pDim = pGen <|> pConstDim

pGen = do
  pResWithNameTok "gen"
  return $ iVar "gen"

pConstDim = do
  val <- pIntLit
  return $ iConst val

pMatStatements = many pMatStatement

pMatStatement = error "pMatStatement" {-do
  pos <- getPosition
  (name, pos) <- pIdent
  pResWithNameTok "="
  ex <- pMatExpr
  pResWithNameTok ";"
  return $ matAsg name ex pos-}

table =
  [[matTransOp],
   [matMulOp, scalMulOp],
   [matAddOp, matSubOp]]

matTransOp = Postfix pMatTransOp
matMulOp = Infix pMatMulOp AssocLeft
scalMulOp = Infix pScalMulOp AssocLeft
matAddOp = Infix pMatAddOp AssocLeft
matSubOp = Infix pMatSubOp AssocLeft

pMatTransOp = do
  pos <- getPosition
  pResWithNameTok "'"
  return $ (\n -> mUnop MTrans n pos)

pScalMulOp = do
  pos <- getPosition
  pResWithNameTok ".*"
  return $ (\l r -> mBinop SMul l r pos)

pMatMulOp = do
  pos <- getPosition
  pResWithNameTok "*"
  return $ (\l r -> mBinop MMul l r pos)
  
pMatAddOp = do
  pos <- getPosition
  pResWithNameTok "+"
  return $ (\l r -> mBinop MAdd l r pos)

pMatSubOp = do
  pos <- getPosition
  pResWithNameTok "-"
  return $ (\l r -> mBinop MSub l r pos)

pTerm = pMatName
      <|> pParens pMatExpr

pMatExpr = buildExpressionParser table pTerm

pMatName = error "pMatName" {-do
  (n, pos) <- pIdent
  return $ matName n pos-}


-}

pParens pOther = do
  pResWithNameTok "("
  ot <- pOther
  pResWithNameTok ")"
  return ot

pIntLit :: Monad m => ParsecT [Token] u m Int
pIntLit = do
  intL <- pIntLitTok
  return $ litInt intL

pIdent :: Monad m => ParsecT [Token] u m (String, SourcePos)
pIdent = do
  position <- getPosition
  idT <- pIdentTok
  return $ (identName idT, position)

pIntLitTok :: Monad m => ParsecT [Token] u m Token
pIntLitTok = mTok (\t -> isIntLit t)

pIdentTok :: Monad m => ParsecT [Token] u m Token
pIdentTok = mTok (\t -> isIdent t)

pResWithNameTok :: Monad m => String -> ParsecT [Token] u m Token
pResWithNameTok name = mTok (\t -> isRes t && resName t == name)

mTok :: (Monad m) => (Token -> Bool) -> ParsecT [Token] u m Token
mTok condition = tokenPrim show updatePos meetsCond
  where
    meetsCond t = if condition t then Just t else Nothing

updatePos :: SourcePos -> Token -> [Token] -> SourcePos
updatePos _ _ (pt:_) = pos pt
updatePos position _ [] = position
