module FrontEnd.Parser({-parseOperation,-}
                       parseStatement,
                       parseFormalParam) where

import Data.Map as M
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

parseStatement :: String -> Map String Matrix -> [Token] -> Either String MatrixStmt
parseStatement sourceFileName matMap toks = case parse (pMatStatement matMap) sourceFileName toks of
  Left err -> Left $ show err
  Right matOp -> Right matOp

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
  s <- pResWithNameTok "scal"
  return (iConst 1, iConst 1, iConst 1, iConst 1)

pVectorL = pRVectorL <|> pCVectorL

pRVectorL = do
  s <- pResWithNameTok "rvec"
  i <- pDim
  return (iConst 1, i, iConst 1, iConst 1)

pCVectorL = do
  s <- pResWithNameTok "cvec"
  i <- pDim
  return (i, iConst 1, iConst 1, iConst 1)

pMatrixL = do
  pResWithNameTok "matx"
  m <- rmLayout <|> cmLayout
  return m

rmLayout = do
  pResWithNameTok "rm"
  nr <- pDim
  nc <- pDim
  return (nr, nc, nc, iConst 1)

cmLayout = do
  pResWithNameTok "cm"
  nr <- pDim
  nc <- pDim
  return (nr, nc, iConst 1, nr)

pDim = pIntConst <|> pVarDim

pIntConst = do
  i <- pIntLit
  return $ iConst i

pVarDim = do
  (i, _) <- pIdent
  return $ iVar i

pScope = do
  scopeStr <- pResWithNameTok "iarg" <|> pResWithNameTok "oarg" <|> pResWithNameTok "temp"
  case resName scopeStr of
    "iarg" -> return arg
    "oarg" -> return arg
    "temp" -> return local

pMatStatement matMap = do
  pos <- getPosition
  w <- pMatrix matMap
  pResWithNameTok "="
  ex <- pMatExpr matMap
  pResWithNameTok ";"
  return $ masg w ex pos

pMatExpr matMap = buildExpressionParser table (pTerm matMap)

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

pTerm matMap = (pMatrix matMap) <|> pParens (pMatExpr matMap)

pMatrix matMap = do
  (n, pos) <- pIdent
  return $ mat (lookupF n matMap) pos {-matName n pos-}

lookupF k m = case M.lookup k m of
  Just v -> v
  Nothing -> error "lookupF: Could not find value"

pOperation = error "pOperation"

{-do
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

pMatStatements = many pMatStatement




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
