import Data.List (intercalate)

-- Types for Parts II and III
data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr
  deriving (Eq, Show, Read)

data LamExpr = LamMacro String 
             | LamApp LamExpr LamExpr 
             | LamAbs Int LamExpr 
             | LamVar Int
  deriving (Eq, Show, Read)

-- Helper function to unparse LamExpr
unparseExpr :: [(String, LamExpr)] -> LamExpr -> String
unparseExpr defs expr = case findMacro defs expr of
  Just name -> name
  Nothing -> case expr of
    LamMacro name -> name -- Macros should never have parentheses
    LamAbs x e -> "\955x" ++ show x ++ "\8594" ++ unparseExpr defs e
    LamApp e1 e2 ->
      let e1Str = if needsParensE1 e1 then "(" ++ unparseExpr defs e1 ++ ")" else unparseExpr defs e1
          e2Str = unparseExpr defs e2 -- Remove unnecessary parentheses for e2
      in e1Str ++ e2Str
    LamVar x -> "x" ++ show x

-- Determine if parentheses are needed around e1 in an application
needsParensE1 :: LamExpr -> Bool
needsParensE1 (LamVar _) = False
needsParensE1 (LamMacro _) = False
needsParensE1 _ = True

-- Determine if parentheses are needed around e2 in an application
needsParensE2 :: LamExpr -> Bool
needsParensE2 (LamApp _ _) = True
needsParensE2 _ = False

-- Recognize macros and replace sub-expressions with macro names
findMacro :: [(String, LamExpr)] -> LamExpr -> Maybe String
findMacro defs expr = lookupExpr expr defs
  where
    lookupExpr _ [] = Nothing
    lookupExpr e ((name, macroExpr):rest)
      | e == macroExpr = Just name
      | otherwise = lookupExpr e rest

-- Recognize macros and replace sub-expressions with macro names
resolveMacros :: [(String, LamExpr)] -> LamExpr -> String
resolveMacros defs expr = case findMacro defs expr of
  Just name -> name
  Nothing -> case expr of
    LamApp e1 e2 ->
      let e1Str = case findMacro defs e1 of
                    Just name -> name
                    Nothing -> if needsParensE1 e1 then "(" ++ resolveMacros defs e1 ++ ")" else resolveMacros defs e1
          e2Str = resolveMacros defs e2
      in e1Str ++ e2Str
    LamAbs x e -> "\955x" ++ show x ++ "\8594" ++ resolveMacros defs e
    LamVar x -> "x" ++ show x
    LamMacro name -> name

-- Main unparse function
unparse :: LamMacroExpr -> String
unparse (LamDef defs e) =
  let defsStr = intercalate "in" 
                  (map (\(name, expr) -> "def" ++ name ++ "=" ++ unparseExpr [] expr) defs)
  in if null defsStr
     then unparseExpr defs e
     else defsStr ++ "in" ++ resolveMacros defs e
