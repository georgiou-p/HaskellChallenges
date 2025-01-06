import Data.Char (isAlphaNum, isSpace, isUpper, isDigit)
import Data.List (nub, isPrefixOf)

-- Abstract syntax for Lambda Calculus with Macros
data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr
  deriving (Eq, Show, Read)

data LamExpr
  = LamMacro String      
  | LamApp LamExpr LamExpr 
  | LamAbs Int LamExpr   
  | LamVar Int        
  deriving (Eq, Show, Read)

parseLamMacro :: String -> Maybe LamMacroExpr
parseLamMacro input =
  let tokens = newTokenizer input  -- Tokenize input string
  in readMacros tokens >>= \(definitions, remainingTokens) ->
       readLambda remainingTokens >>= \(bodyExpr, remaining) ->
         case (remaining, macrosAreValid definitions) of
           ([], True)  -> Just (LamDef definitions bodyExpr) -- Build AST if valid
           _           -> Nothing -- Invalid macros or unparsed input

-- Helper function to parse chains of applications
chainApp :: LamExpr -> [String] -> Maybe (LamExpr, [String])
chainApp current tokens =
  case tokens of
    ("(" : rest) ->
      readLambda rest >>= \(nextExpr, remaining) ->
        case remaining of
          ")" : rest2 -> chainApp (LamApp current nextExpr) rest2 -- Parse nested applications
          _ -> Nothing -- Mismatched parentheses
    (token : rest)
      | isVariable token -> chainApp (LamApp current (LamVar (read (drop 1 token)))) rest
      | isMacro token    -> chainApp (LamApp current (LamMacro token)) rest
      | otherwise        -> Just (current, token : rest)
    _ -> Just (current, tokens) -- End of tokens
  where
    isVariable t = "x" `isPrefixOf` t -- Variables start with "x" and are followed by digits
    isMacro t    = all isUpper t      -- Macros are uppercase strings

-- Tokenizer for input string
newTokenizer :: String -> [String]
newTokenizer [] = []
newTokenizer (x:xs)
  | x `elem` "()='→λ" = [x] : newTokenizer xs -- Single-character tokens
  | isSpace x         = newTokenizer xs      -- Ignore whitespace
  | otherwise =
      let fragment = x : takeWhile isAlphaNum xs
      in fragment : newTokenizer (dropWhile isAlphaNum xs) -- Alphanumeric fragments

-- Recursive parsing of lambda expressions
readLambda :: [String] -> Maybe (LamExpr, [String])
readLambda tokens =
  case tokens of
    ("λ" : var : "→" : rest) ->
      readLambda rest >>= \(subExpr, remaining) ->
        chainApp (LamAbs (read (drop 1 var)) subExpr) remaining -- Parse lambda abstraction
    ("(" : rest) ->
      readLambda rest >>= \(innerExpr, remainingTokens) ->
        case remainingTokens of
          ")" : rest2 -> chainApp innerExpr rest2 -- Parse parentheses
          _ -> Nothing -- Mismatched parentheses
    (token : rest)
      | isVariable token -> chainApp (LamVar (read (drop 1 token))) rest
      | isMacro token    -> chainApp (LamMacro token) rest
      | otherwise        -> Nothing -- Invalid token
    [] -> Nothing -- End of input
  where
    isVariable tok = "x" `isPrefixOf` tok && validVariable tok -- Check if valid variable
    validVariable var =
      let digits = drop 1 var
      in not (null digits) && all isDigit digits -- Variable should have digits after "x"
    isMacro tok = all isUpper tok -- Macros are all uppercase

-- Check if an expression is closed (no free variables)
isExpressionClosed :: LamExpr -> Bool
isExpressionClosed expr = isClosed expr []
  where
    isClosed :: LamExpr -> [Int] -> Bool
    isClosed (LamVar idx) bound = idx `elem` bound
    isClosed (LamAbs param body) bound = isClosed body (param : bound)
    isClosed (LamApp left right) bound = all (`isClosed` bound) [left, right]
    isClosed (LamMacro _) _ = True


-- Parse macros and their definitions
readMacros :: [String] -> Maybe ([(String, LamExpr)], [String])
readMacros tokens =
  case tokens of
    ("def" : macroName : "=" : rest) ->
      readLambda rest >>= \(macroExpr, remainingTokens) ->
        case remainingTokens of
          "in" : furtherTokens ->
            case furtherTokens of
              ("def" : _) -> Nothing -- Nested macro definitions are invalid
              _ -> readMacros furtherTokens >>= \(otherDefs, leftover) ->
                     Just ((macroName, macroExpr) : otherDefs, leftover)
          _ -> Nothing -- Invalid macro definition syntax
    _ -> Just ([], tokens) -- No macros found

-- Validate macro definitions
macrosAreValid :: [(String, LamExpr)] -> Bool
macrosAreValid macros =
  let names = map fst macros
      uniqueNames = nub names -- Ensure unique macro names
      allClosed = all (isExpressionClosed . snd) macros -- Ensure macros are closed
  in length names == length uniqueNames && allClosed
