-- DO NOT MODIFY THESE DATATYPES
data LamMacroExpr = LamDef [(String, LamExpr)] LamExpr
  deriving (Eq, Show, Read)

data LamExpr
  = LamMacro String
  | LamApp  LamExpr LamExpr
  | LamAbs  Int     LamExpr
  | LamVar  Int
  deriving (Eq, Show, Read)

-- | Direct CPS transform of a single LamExpr using fixed variable indices:
--   0 for the top-level continuation, 1 for the function, 2 for the argument, etc.
directCPS :: LamExpr -> LamExpr
directCPS (LamVar x) =
  LamAbs 0 (LamApp (LamVar 0) (LamVar x))

directCPS (LamAbs x e) =
  LamAbs 0 (LamApp (LamVar 0) (LamAbs x (directCPS e)))

directCPS (LamApp e1 e2) =
  LamAbs 0 $
    LamApp
      (directCPS e1)
      (LamAbs 1 $
         LamApp
           (directCPS e2)
           (LamAbs 2 $
              LamApp
                (LamApp (LamVar 1) (LamVar 2))
                (LamVar 0)
           )
      )

directCPS (LamMacro name) =
  LamMacro name

-- | Transform a single macro definition by applying directCPS to its body
cpsTransformDef :: (String, LamExpr) -> (String, LamExpr)
cpsTransformDef (name, body) = (name, directCPS body)

-- | Transform an entire LamMacroExpr by applying directCPS to all definitions and the main expression
cpsTransform :: LamMacroExpr -> LamMacroExpr
cpsTransform (LamDef defs mainExpr) =
  LamDef (map cpsTransformDef defs) (directCPS mainExpr)
