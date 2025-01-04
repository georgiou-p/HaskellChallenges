import Control.Monad.State
import Data.List (nub, isPrefixOf, elemIndex)
import Data.Char (isDigit, isAlphaNum)
import Control.Applicative ((<|>))

-- Represents a Lambda macro expression with macros and a body.
data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr
  deriving (Eq, Show, Read)

-- Represents a Lambda expression with macros, applications, abstractions, and variables.
data LamExpr
  = LamMacro String
  | LamApp LamExpr LamExpr
  | LamAbs Int LamExpr
  | LamVar Int
  deriving (Eq, Show, Read)

-- Utilities for variables and substitution

-- Checks if a variable is free in the expression.
isFree :: Int -> LamExpr -> Bool
isFree v (LamAbs v' e1) = v /= v' && isFree v e1
isFree v (LamApp x y)   = isFree v x && isFree v y
isFree v (LamVar vv)    = v == vv
isFree _ (LamMacro _)   = True

-- Finds the highest variable index used in the expression.
highestVariable :: LamExpr -> Int
highestVariable (LamVar n)   = n
highestVariable (LamAbs v e) = max v (highestVariable e)
highestVariable (LamApp p q) = max (highestVariable p) (highestVariable q)
highestVariable (LamMacro _) = 0

-- Substitutes all occurrences of a variable with another expression.
subs :: LamExpr -> Int -> LamExpr -> LamExpr
subs (LamVar y) x arg
  | y == x    = arg
  | otherwise = LamVar y
subs abs@(LamAbs v1 e1) v2 e2
  | v1 == v2  = abs
  | isFree v1 e2 =
      let v1'  = 1 + maximum (map highestVariable [abs, e2])
          body = subs e1 v1 (LamVar v1')
      in subs (LamAbs v1' body) v2 e2
  | otherwise = LamAbs v1 (subs e1 v2 e2)
subs (LamApp e1 e2) v e = LamApp (subs e1 v e) (subs e2 v e)
subs m@(LamMacro _) _ _ = m

-- Replaces macros with their definitions wherever they appear.
macroReplace :: (String, LamExpr) -> LamExpr -> LamExpr
macroReplace (name, def) (LamMacro s)
  | s == name = def
  | otherwise = LamMacro s
macroReplace _ (LamVar n)     = LamVar n
macroReplace s (LamAbs v e1)  = LamAbs v (macroReplace s e1)
macroReplace s (LamApp l r)   = LamApp (macroReplace s l) (macroReplace s r)


-- Beta reduction for Lambda expressions

-- Performs one step of outermost beta reduction.
reduceOutermostOneStep :: LamExpr -> Maybe LamExpr
reduceOutermostOneStep (LamAbs v e1) = LamAbs v <$> reduceOutermostOneStep e1
reduceOutermostOneStep (LamApp (LamAbs v e1) e2) = Just (subs e1 v e2)
reduceOutermostOneStep (LamApp e1 e2) =
      (LamApp <$> reduceOutermostOneStep e1 <*> pure e2)
  <|> (LamApp e1 <$> reduceOutermostOneStep e2)
reduceOutermostOneStep _ = Nothing

-- Performs one step of innermost beta reduction.
reduceInnermostOneStep :: LamExpr -> Maybe LamExpr
reduceInnermostOneStep (LamApp e1 e2) =
      (LamApp <$> reduceInnermostOneStep e1 <*> pure e2)
  <|> (LamApp e1 <$> reduceInnermostOneStep e2)
  <|> case e1 of
       LamAbs v absExpr -> Just (subs absExpr v e2)
       _                -> Nothing
reduceInnermostOneStep (LamAbs v bdy) = LamAbs v <$> reduceInnermostOneStep bdy
reduceInnermostOneStep _ = Nothing

-- Applying reductions to Lambda macro expressions
-- Applies one step of outermost reduction to a macro expression.
outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
outerRedn1 (LamDef defs e1) =
  case defs of
    (m:ms) ->
      let newDefs = map (\(nm, df) -> (nm, macroReplace m df)) ms
          e1'     = macroReplace m e1
      in Just (LamDef newDefs e1')
    [] -> LamDef [] <$> reduceOutermostOneStep e1

-- Applies one step of innermost reduction to a macro expression.
innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr
innerRedn1 (LamDef (m:ms) e) =
      ( do LamDef ms' e' <- innerRedn1 (LamDef ms e)
           return (LamDef (m : ms') e')
      ) <|> Just (LamDef [] (macroReplace m e))
innerRedn1 (LamDef [] e) = LamDef [] <$> reduceInnermostOneStep e

-- Counting reduction steps and comparing inner vs. outer reductions

-- Counts the number of steps until no more reductions are possible.
countSteps :: (LamMacroExpr -> Maybe LamMacroExpr) -> Int
           -> LamMacroExpr -> Maybe Int
countSteps evalFunc limit e =
  let reductions = tail (iterate (>>= evalFunc) (Just e))
  in elemIndex Nothing (take (limit + 1) reductions)

-- Compares inner and outer reductions for a macro expression.
compareInnerOuter :: LamMacroExpr -> Int
                  -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)
compareInnerOuter e limit =
  ( countInner e
  , countOuter e
  , countInner (toCps e)
  , countOuter (toCps e)
  )
  where
    countInner = countSteps innerRedn1 limit
    countOuter = countSteps outerRedn1 limit

-- Converts a macro expression to CPS.
toCps :: LamMacroExpr -> LamMacroExpr
toCps def =
  let LamDef macros e' = cpsTransform def
  in LamDef macros (LamApp e' identExpr)

-- Transforms a macro expression into CPS.
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform expr@(LamDef defs body) =
  let maxIndex = macroMaxVarIndex expr
      (LamDef defs' body', _) = runState (cpsMacroExpr (LamDef defs body)) (maxIndex + 1)
  in LamDef defs' body'

-- Finds the highest variable index in the macro expression.
macroMaxVarIndex :: LamMacroExpr -> Int
macroMaxVarIndex (LamDef defs bod) =
  let allExprs = bod : map snd defs
  in maximum (0 : map maxVarIndex allExprs)

-- Finds the highest variable index in a Lambda expression.
maxVarIndex :: LamExpr -> Int
maxVarIndex (LamVar x)   = x
maxVarIndex (LamAbs x b) = max x (maxVarIndex b)
maxVarIndex (LamApp l r) = max (maxVarIndex l) (maxVarIndex r)
maxVarIndex (LamMacro _) = 0

-- Converts each macro and the body of a macro expression into CPS.
cpsMacroExpr :: LamMacroExpr -> State Int LamMacroExpr
cpsMacroExpr (LamDef defs bod) = do
  defs' <- mapM (\(nm, e) -> cpsExpr e >>= (return . (nm,))) defs
  bod'  <- cpsExpr bod
  return (LamDef defs' bod')

-- Converts a Lambda expression into CPS.
cpsExpr :: LamExpr -> State Int LamExpr
cpsExpr (LamVar x) = do
  k <- freshVariable
  return (LamAbs k (LamApp (LamVar k) (LamVar x)))
cpsExpr (LamMacro name) = return (LamMacro name)
cpsExpr (LamAbs x body) = do
  k <- freshVariable
  body' <- cpsExpr body
  return (LamAbs k (LamApp (LamVar k) (LamAbs x body')))
cpsExpr (LamApp e1 e2) = do
  k <- freshVariable
  f <- freshVariable
  e <- freshVariable
  e1' <- cpsExpr e1
  e2' <- cpsExpr e2
  let inner = LamAbs f (LamApp e2' (LamAbs e (LamApp (LamApp (LamVar f) (LamVar e))
                                                   (LamVar k))))
  return (LamAbs k (LamApp e1' inner))

-- Generates a fresh variable in the State monad.
freshVariable :: State Int Int
freshVariable = do
  n <- get
  put (n + 1)
  return n

-- Represents the identity function in Lambda calculus.
identExpr :: LamExpr
identExpr = LamAbs 0 (LamVar 0)
