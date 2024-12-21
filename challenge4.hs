import Data.Char (isDigit, isAlpha)
import Text.ParserCombinators.ReadP
import Control.Applicative ((<|>))

data LamMacroExpr
  = LamDef [(String, LamExpr)] LamExpr
  deriving (Eq, Show, Read)

data LamExpr
  = LamMacro String
  | LamApp LamExpr LamExpr
  | LamAbs Int LamExpr
  | LamVar Int
  deriving (Eq, Show, Read)

type Parser a = ReadP a

ws :: Parser ()
ws = skipSpaces

parseVar :: Parser LamExpr
parseVar = do
  _ <- char 'x'
  n <- munch1 isDigit
  return (LamVar (read n))

parseMacro :: Parser LamExpr
parseMacro = do
  name <- munch1 isAlpha
  if all (`elem` ['A'..'Z']) name
    then return (LamMacro name)
    else pfail

parseAbs :: Parser LamExpr
parseAbs = do
  _ <- char '\\' <|> char 'λ'
  ws
  _ <- char 'x'
  n <- munch1 isDigit
  ws
  _ <- string "->" <|> string "→"
  ws
  body <- parseExpr
  return (LamAbs (read n) body)

parseSimpleExpr :: Parser LamExpr
parseSimpleExpr =
      parseVar
  <|> parseMacro
  <|> between (char '(' >> ws) (ws >> char ')') parseExpr

parseApplication :: Parser LamExpr
parseApplication = do
  first <- parseSimpleExpr
  rest  <- many (ws *> parseSimpleExpr)
  return (foldl LamApp first rest)

parseExpr :: Parser LamExpr
parseExpr = parseAbs <|> parseApplication

definition :: [(String, LamExpr)] -> Parser (String, LamExpr)
definition macros = do
  _    <- string "def"
  ws
  name <- munch1 isAlpha
  ws
  _    <- char '='
  ws
  rhs  <- parseExpr
  ws
  if isClosed rhs macros then return (name, rhs) else pfail

isClosed :: LamExpr -> [(String, LamExpr)] -> Bool
isClosed e ms = null (freeVars e [] ms)

freeVars :: LamExpr -> [Int] -> [(String, LamExpr)] -> [Int]
freeVars (LamVar x) bound _      = [ x | x `notElem` bound ]
freeVars (LamAbs x b) bound ms   = freeVars b (x:bound) ms
freeVars (LamApp a b) bound ms   = freeVars a bound ms ++ freeVars b bound ms
freeVars (LamMacro n) _     ms   =
  case lookup n ms of
    Just body -> freeVars body [] ms
    Nothing   -> []

parseLamMacroExpr :: [(String, LamExpr)] -> Parser LamMacroExpr
parseLamMacroExpr ms = do
  defPart <- option Nothing (Just <$> (definition ms <* string "in" <* ws))
  case defPart of
    Just (name, rhs) -> do
      rest <- parseLamMacroExpr ((name, rhs) : ms)
      case rest of
        LamDef defs body -> return (LamDef ((name, rhs) : defs) body)
    Nothing -> do
      e <- parseExpr
      return (LamDef ms e)

uniqueMacros :: [(String, LamExpr)] -> Bool
uniqueMacros defs =
  let names = map fst defs
  in length names == length (foldr (\x acc -> if x `elem` acc then acc else x:acc) [] names)

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro input =
  case readP_to_S (ws *> parseLamMacroExpr [] <* eof) input of
    [(LamDef defs expr, "")] ->
      if uniqueMacros defs
        then Just (LamDef defs expr)
        else Nothing
    _ -> Nothing
