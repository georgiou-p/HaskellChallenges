module Main where

import Data.List (sort)
import qualified Challenge1 as C1 -- Qualified import in order to not chnage the function names 
import qualified Challenge2 as C2 
import qualified Challenge3 as C3 
import qualified Challenge4 as C4
import qualified Challenge5 as C5
import qualified Challenge6 as C6

-- Helper function for tests
checkTest :: (Eq a, Show a) => String -> a -> a -> IO ()
checkTest testName actual expected =
  if actual == expected
    then putStrLn (testName ++ ": PASS")
    else putStrLn (testName ++ ": **FAIL**")

----------------------------------
------TESTS FOR CHALLENGE 1-------
----------------------------------
-- Test 1: No atoms in a small grid
test1 :: IO ()
test1 = do
  let gridSize = 2
      atoms = []
      result = sort (C1.calcInteractions gridSize atoms)
      expectedCount = 12 -- Total rays: 3 faces * 2 rows * 2 directions
  checkTest "Test1" (length result) expectedCount

-- Test 2: Single atom in a 3x3 grid
test2 :: IO ()
test2 = do
  let gridSize = 3
      atoms = [(2, 2)]
      result = sort (C1.calcInteractions gridSize atoms)
      expectedCount = 18 -- Manually verified expected count
  checkTest "Test2" (length result) expectedCount

-- Test 3: Example from problem statement
test3 :: IO ()
test3 = do
  let gridSize = 6
      atoms = [(2, 3), (4, 3), (4, 6), (5, 5)]
      actual = sort (C1.calcInteractions gridSize atoms)
      expected = sort
        [ (C1.EP C1.East 1 C1.L, C1.EP C1.West 1 C1.R)
        , (C1.EP C1.East 1 C1.R, C1.EP C1.South 1 C1.L)
        , (C1.EP C1.East 2 C1.L, C1.EP C1.East 2 C1.R)
        , (C1.EP C1.East 2 C1.R, C1.EP C1.East 2 C1.L)
        , (C1.EP C1.East 3 C1.L, C1.EP C1.West 3 C1.R)
        , (C1.EP C1.East 3 C1.R, C1.EP C1.East 5 C1.L)
        , (C1.EP C1.East 4 C1.L, C1.EP C1.South 6 C1.R)
        , (C1.EP C1.East 4 C1.R, C1.EP C1.South 4 C1.L)
        , (C1.EP C1.East 5 C1.L, C1.EP C1.East 3 C1.R)
        , (C1.EP C1.East 5 C1.R, C1.EP C1.South 5 C1.L)
        , (C1.EP C1.East 6 C1.L, C1.EP C1.West 6 C1.R)
        , (C1.EP C1.East 6 C1.R, C1.EP C1.South 6 C1.L)
        , (C1.EP C1.West 1 C1.L, C1.EP C1.West 2 C1.R)
        , (C1.EP C1.West 1 C1.R, C1.EP C1.East 1 C1.L)
        , (C1.EP C1.West 2 C1.L, C1.EP C1.South 5 C1.R)
        , (C1.EP C1.West 2 C1.R, C1.EP C1.West 1 C1.L)
        , (C1.EP C1.West 3 C1.L, C1.EP C1.West 4 C1.R)
        , (C1.EP C1.West 3 C1.R, C1.EP C1.East 3 C1.L)
        , (C1.EP C1.West 4 C1.L, C1.EP C1.South 3 C1.R)
        , (C1.EP C1.West 4 C1.R, C1.EP C1.West 3 C1.L)
        , (C1.EP C1.West 5 C1.L, C1.EP C1.South 2 C1.R)
        , (C1.EP C1.West 5 C1.R, C1.EP C1.South 2 C1.L)
        , (C1.EP C1.West 6 C1.L, C1.EP C1.South 1 C1.R)
        , (C1.EP C1.West 6 C1.R, C1.EP C1.East 6 C1.L)
        , (C1.EP C1.South 1 C1.L, C1.EP C1.East 1 C1.R)
        , (C1.EP C1.South 1 C1.R, C1.EP C1.West 6 C1.L)
        , (C1.EP C1.South 2 C1.L, C1.EP C1.West 5 C1.R)
        , (C1.EP C1.South 2 C1.R, C1.EP C1.West 5 C1.L)
        , (C1.EP C1.South 3 C1.L, C1.EP C1.South 4 C1.R)
        , (C1.EP C1.South 3 C1.R, C1.EP C1.West 4 C1.L)
        , (C1.EP C1.South 4 C1.L, C1.EP C1.East 4 C1.R)
        , (C1.EP C1.South 4 C1.R, C1.EP C1.South 3 C1.L)
        , (C1.EP C1.South 5 C1.L, C1.EP C1.East 5 C1.R)
        , (C1.EP C1.South 5 C1.R, C1.EP C1.West 2 C1.L)
        , (C1.EP C1.South 6 C1.L, C1.EP C1.East 6 C1.R)
        , (C1.EP C1.South 6 C1.R, C1.EP C1.East 4 C1.L)
        ]
  checkTest "Test3" actual expected

-- Test 4: Single atom at the boundary of a 2x2 grid
test4 :: IO ()
test4 = do
  let gridSize = 2
      atoms = [(1, 1)]
      result = sort (C1.calcInteractions gridSize atoms)
      expectedCount = 12 -- Rays interact but total count should remain constant
  checkTest "Test4" (length result) expectedCount

-- Test 5: Ray immediately reflects on entry
test5 :: IO ()
test5 = do
  let gridSize = 3
      atoms = [(1, 1)] -- Atom directly in the path of an entering ray
      result = sort (C1.calcInteractions gridSize atoms)
      expectedCount = 18 -- Some rays reflect but overall count is unaffected
  checkTest "Test5" (length result) expectedCount

-- Test 6: Larger grid with multiple reflections
test6 :: IO ()
test6 = do
  let gridSize = 5
      atoms = [(2, 2), (3, 4), (4, 2)]
      result = sort (C1.calcInteractions gridSize atoms)
      expectedCount = 30 -- Manually calculated total expected interactions
  checkTest "Test6" (length result) expectedCount

----------------------------------
------TESTS FOR CHALLENGE 2-------
----------------------------------

-- Test 1: Single Atom in a Small Grid
-- Purpose: To verify that the function correctly identifies the single atom's location in a 3x3 grid.
testChallenge2_1 :: IO ()
testChallenge2_1 = do
  let nAtoms = 1 -- Number of atoms
      interactions = C2.calcInteractions 3 [(2, 2)] -- Interactions based on a single atom at (2, 2)
      expected = [(2, 2)] 
      result = C2.solveTBB nAtoms interactions 
  checkTest "TestChallenge2_1" result expected

-- Test 2: Multiple Atoms in a Medium Grid
-- Purpose: To test if the function can identify two atoms in a 4x4 grid.
testChallenge2_2 :: IO ()
testChallenge2_2 = do
  let nAtoms = 2 
      interactions = C2.calcInteractions 4 [(2, 3), (3, 4)] -- Interactions for two atoms at (2, 3) and (3, 4)
      expected = [(2, 3), (3, 4)] -- Expected output: both atom locations
      result = C2.solveTBB nAtoms interactions 
  checkTest "TestChallenge2_2" result expected

-- Test 3: Complex Grid with Multiple Atoms
-- Purpose: To ensure the function works with a larger grid (6x6) and multiple atoms (4 atoms).
testChallenge2_3 :: IO ()
testChallenge2_3 = do
  let nAtoms = 4 -- Number of atoms
      interactions = C2.calcInteractions 6 [(2, 3), (4, 5), (5, 2), (6, 6)] 
      expected = [(2, 3), (4, 5), (5, 2), (6, 6)] 
      result = C2.solveTBB nAtoms interactions 
  checkTest "TestChallenge2_3" (sort result) (sort expected)

-- Test 4: Edge Case with No Atoms
-- Purpose: To check if the function handles an empty grid (no atoms) correctly.
testChallenge2_4 :: IO ()
testChallenge2_4 = do
  let nAtoms = 0 -- No atoms
      interactions = C2.calcInteractions 5 [] -- Interactions for an empty grid
      expected = [] -- Expected output: no atom locations
      result = C2.solveTBB nAtoms interactions 
  checkTest "TestChallenge2_4" result expected

-- Test 5: Multiple Valid Solutions
-- Purpose: To test a scenario where multiple valid solutions exist and ensure one valid solution is returned.
testChallenge2_5 :: IO ()
testChallenge2_5 = do
  let nAtoms = 3 -- Number of atoms
      interactions = C2.calcInteractions 5 [(2, 2), (3, 4), (4, 3)] -- Interactions for 3 atoms
      -- Valid solutions (order doesn't matter)
      validSolutions = [[(2, 2), (3, 4), (4, 3)], [(4, 3), (2, 2), (3, 4)]]
      result = C2.solveTBB nAtoms interactions 
  checkTest "TestChallenge2_5" (result `elem` validSolutions) True

-- Test 6: Larger Grid with Specific Interactions
-- Purpose: To test `solveTBB` with a more complex grid (8x8) and 4 atoms, ensuring it correctly finds their locations.
testChallenge2_6 :: IO ()
testChallenge2_6 = do
  let nAtoms = 4 -- Number of atoms
      interactions = 
        [ (C2.EP C2.East 1 C2.L, C2.EP C2.West 1 C2.R)
        , (C2.EP C2.East 1 C2.R, C2.EP C2.East 5 C2.L)
        , (C2.EP C2.East 2 C2.L, C2.EP C2.West 2 C2.R)
        , (C2.EP C2.East 2 C2.R, C2.EP C2.East 4 C2.L)
        , (C2.EP C2.East 3 C2.L, C2.EP C2.West 3 C2.R)
        , (C2.EP C2.East 3 C2.R, C2.EP C2.South 3 C2.L)
        , (C2.EP C2.East 4 C2.L, C2.EP C2.East 2 C2.R)
        , (C2.EP C2.East 4 C2.R, C2.EP C2.East 6 C2.L)
        , (C2.EP C2.East 5 C2.L, C2.EP C2.East 1 C2.R)
        , (C2.EP C2.East 5 C2.R, C2.EP C2.South 5 C2.L)
        , (C2.EP C2.East 6 C2.L, C2.EP C2.East 4 C2.R)
        , (C2.EP C2.East 6 C2.R, C2.EP C2.West 2 C2.L)
        , (C2.EP C2.East 7 C2.L, C2.EP C2.West 7 C2.R)
        , (C2.EP C2.East 7 C2.R, C2.EP C2.South 7 C2.L)
        , (C2.EP C2.East 8 C2.L, C2.EP C2.South 7 C2.R)
        , (C2.EP C2.East 8 C2.R, C2.EP C2.South 8 C2.L)
        , (C2.EP C2.West 1 C2.L, C2.EP C2.South 8 C2.R)
        , (C2.EP C2.West 1 C2.R, C2.EP C2.East 1 C2.L)
        , (C2.EP C2.West 2 C2.L, C2.EP C2.East 6 C2.R)
        , (C2.EP C2.West 2 C2.R, C2.EP C2.East 2 C2.L)
        , (C2.EP C2.West 3 C2.L, C2.EP C2.West 4 C2.R)
        , (C2.EP C2.West 3 C2.R, C2.EP C2.East 3 C2.L)
        , (C2.EP C2.West 4 C2.L, C2.EP C2.South 5 C2.R)
        , (C2.EP C2.West 4 C2.R, C2.EP C2.West 3 C2.L)
        , (C2.EP C2.West 5 C2.L, C2.EP C2.West 5 C2.R)
        , (C2.EP C2.West 5 C2.R, C2.EP C2.West 5 C2.L)
        , (C2.EP C2.West 6 C2.L, C2.EP C2.South 3 C2.R)
        , (C2.EP C2.West 6 C2.R, C2.EP C2.South 2 C2.L)
        , (C2.EP C2.West 7 C2.L, C2.EP C2.South 2 C2.R)
        , (C2.EP C2.West 7 C2.R, C2.EP C2.East 7 C2.L)
        , (C2.EP C2.West 8 C2.L, C2.EP C2.South 1 C2.R)
        , (C2.EP C2.West 8 C2.R, C2.EP C2.South 6 C2.L)
        , (C2.EP C2.South 1 C2.L, C2.EP C2.South 4 C2.R)
        , (C2.EP C2.South 1 C2.R, C2.EP C2.West 8 C2.L)
        , (C2.EP C2.South 2 C2.L, C2.EP C2.West 6 C2.R)
        , (C2.EP C2.South 2 C2.R, C2.EP C2.West 7 C2.L)
        , (C2.EP C2.South 3 C2.L, C2.EP C2.East 3 C2.R)
        , (C2.EP C2.South 3 C2.R, C2.EP C2.West 6 C2.L)
        , (C2.EP C2.South 4 C2.L, C2.EP C2.South 6 C2.R)
        , (C2.EP C2.South 4 C2.R, C2.EP C2.South 1 C2.L)
        , (C2.EP C2.South 5 C2.L, C2.EP C2.East 5 C2.R)
        , (C2.EP C2.South 5 C2.R, C2.EP C2.West 4 C2.L)
        , (C2.EP C2.South 6 C2.L, C2.EP C2.West 8 C2.R)
        , (C2.EP C2.South 6 C2.R, C2.EP C2.South 4 C2.L)
        , (C2.EP C2.South 7 C2.L, C2.EP C2.East 7 C2.R)
        , (C2.EP C2.South 7 C2.R, C2.EP C2.East 8 C2.L)
        , (C2.EP C2.South 8 C2.L, C2.EP C2.East 8 C2.R)
        , (C2.EP C2.South 8 C2.R, C2.EP C2.West 1 C2.L)
        ]
      expected = [(4, 3), (5, 1), (6, 7), (8, 12)] -- Expected atom locations
      result = C2.solveTBB nAtoms interactions -- Solve using Challenge2's solveTBB
  checkTest "TestChallenge2_6" result expected


----------------------------------
------TESTS FOR CHALLENGE 3-------
----------------------------------

-- Test 1: Simple lambda application with no macros
-- Purpose: To ensure that the function handles lambda application correctly without macros.
testChallenge3_1 :: IO ()
testChallenge3_1 = do
  let expr = C3.LamDef [] (C3.LamApp (C3.LamAbs 1 (C3.LamVar 1)) (C3.LamAbs 1 (C3.LamVar 1)))
      expected = "(\955x1\8594x1)\955x1\8594x1" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_1" result expected

-- Test 2: Lambda abstraction with nested application
-- Purpose: To verify correct handling of nested applications in lambda abstractions.
testChallenge3_2 :: IO ()
testChallenge3_2 = do
  let expr = C3.LamDef [] (C3.LamAbs 1 (C3.LamApp (C3.LamVar 1) (C3.LamAbs 1 (C3.LamVar 1))))
      expected = "\955x1\8594x1\955x1\8594x1" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_2" result expected

-- Test 3: Lambda with a single macro definition
-- Purpose: To ensure macros are recognized and correctly replaced in the output.
testChallenge3_3 :: IO ()
testChallenge3_3 = do
  let expr = C3.LamDef [("F", C3.LamAbs 1 (C3.LamVar 1))] (C3.LamAbs 2 (C3.LamApp (C3.LamVar 2) (C3.LamMacro "F")))
      expected = "defF=\955x1\8594x1in\955x2\8594x2F" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_3" result expected

-- Test 4: Lambda with macro and nested abstraction
-- Purpose: To test macros and nested abstractions together.
testChallenge3_4 :: IO ()
testChallenge3_4 = do
  let expr = C3.LamDef [("F", C3.LamAbs 1 (C3.LamVar 1))] (C3.LamAbs 2 (C3.LamApp (C3.LamAbs 1 (C3.LamVar 1)) (C3.LamVar 2)))
      expected = "defF=\955x1\8594x1in\955x2\8594Fx2" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_4" result expected

-- Test 5: Multiple macros with overlapping definitions
-- Purpose: To ensure the correct macro is chosen when definitions overlap.
testChallenge3_5 :: IO ()
testChallenge3_5 = do
  let expr = C3.LamDef 
                [("G", C3.LamAbs 1 (C3.LamAbs 2 (C3.LamVar 1))),
                 ("F", C3.LamAbs 1 (C3.LamVar 1))]
                (C3.LamAbs 2 (C3.LamApp (C3.LamMacro "G") (C3.LamMacro "F")))
      expected = "defG=\955x1\8594\955x2\8594x1indefF=\955x1\8594x1in\955x2\8594GF" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_5" result expected

-- Test 6: Multiple macros without usage in the main expression
-- Purpose: To ensure macros are defined correctly even if unused in the main expression.
testChallenge3_6 :: IO ()
testChallenge3_6 = do
  let expr = C3.LamDef 
               [("A", C3.LamAbs 1 (C3.LamVar 1)), 
                ("B", C3.LamAbs 2 (C3.LamApp (C3.LamVar 2) (C3.LamVar 2)))]
               (C3.LamAbs 3 (C3.LamVar 3))
      expected = "defA=\955x1\8594x1indefB=\955x2\8594x2x2in\955x3\8594x3" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_6" result expected

-- Test 7: Single variable expression
-- Purpose: To verify the function can handle the simplest lambda expression with a single variable.
testChallenge3_7 :: IO ()
testChallenge3_7 = do
  let expr = C3.LamDef [] (C3.LamVar 1) -- Single variable x1
      expected = "x1" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_7" result expected

-- Test 8: Simple abstraction with no macros
-- Purpose: To ensure correct handling of a single lambda abstraction without macros.
testChallenge3_8 :: IO ()
testChallenge3_8 = do
  let expr = C3.LamDef [] (C3.LamAbs 1 (C3.LamVar 1)) -- Lambda abstraction: λx1 → x1
      expected = "\955x1\8594x1" -- Expected unparsed string
      result = C3.unparse expr
  checkTest "TestChallenge3_8" result expected

----------------------------------
------TESTS FOR CHALLENGE 4-------
----------------------------------

-- Test 1: Simple valid lambda expression
testChallenge4_1 :: IO ()
testChallenge4_1 = do
  let input = "x1 (x2 x3)"
      expected = Just (C4.LamDef [] (C4.LamApp (C4.LamVar 1) (C4.LamApp (C4.LamVar 2) (C4.LamVar 3))))
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_1" result expected

-- Test 2: Valid expression with a macro
testChallenge4_2 :: IO ()
testChallenge4_2 = do
  let input = "x1 x2 F"
      expected = Just (C4.LamDef [] (C4.LamApp (C4.LamApp (C4.LamVar 1) (C4.LamVar 2)) (C4.LamMacro "F")))
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_2" result expected

-- Test 3: Valid expression with a defined macro
testChallenge4_3 :: IO ()
testChallenge4_3 = do
  let input = "def F = λx1 → x1 in λx2 → x2 F"
      expected = Just (C4.LamDef [("F", C4.LamAbs 1 (C4.LamVar 1))] (C4.LamAbs 2 (C4.LamApp (C4.LamVar 2) (C4.LamMacro "F"))))
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_3" result expected

-- Test 4: Invalid nested macro definitions
testChallenge4_4 :: IO ()
testChallenge4_4 = do
  let input = "def F = λx1 → x1 ( def G = λx1 → x1 in x1) in λx2 → x2"
      expected = Nothing
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_4" result expected

-- Test 5: Invalid repeated macro definitions
testChallenge4_5 :: IO ()
testChallenge4_5 = do
  let input = "def F = λx1 → x1 in def F = λx2 → x2 x1 in x1"
      expected = Nothing
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_5" result expected

-- Test 6: Macro body not closed
testChallenge4_6 :: IO ()
testChallenge4_6 = do
  let input = "def F = x1 in F"
      expected = Nothing
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_6" result expected

-- Test 7: Valid expression with multiple macros
testChallenge4_7 :: IO ()
testChallenge4_7 = do
  let input = "def F = λx1 → x1 in def G = λx2 → x2 in F G"
      expected = Just (C4.LamDef [("F", C4.LamAbs 1 (C4.LamVar 1)), ("G", C4.LamAbs 2 (C4.LamVar 2))] (C4.LamApp (C4.LamMacro "F") (C4.LamMacro "G")))
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_7" result expected

-- Test 8: Valid expression with nested applications
testChallenge4_8 :: IO ()
testChallenge4_8 = do
  let input = "(λx1 → x1) x2"
      expected = Just (C4.LamDef [] (C4.LamApp (C4.LamAbs 1 (C4.LamVar 1)) (C4.LamVar 2)))
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_8" result expected

-- Test 9: Invalid expression with unmatched parentheses
testChallenge4_9 :: IO ()
testChallenge4_9 = do
  let input = "(x1 (x2 x3)"
      expected = Nothing
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_9" result expected

-- Test 10: Empty input
testChallenge4_10 :: IO ()
testChallenge4_10 = do
  let input = ""
      expected = Nothing
      result = C4.parseLamMacro input
  checkTest "TestChallenge4_10" result expected

----------------------------------
------TESTS FOR CHALLENGE 5-------
----------------------------------

-- Test 1: Simple application in CPS
testChallenge5_1 :: IO ()
testChallenge5_1 = do
  let expr = C5.LamDef [] (C5.LamApp (C5.LamVar 1) (C5.LamVar 2))
      expected = C5.LamDef [] (C5.LamAbs 0 (C5.LamApp (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamVar 1))) (C5.LamAbs 1 (C5.LamApp (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamVar 2))) (C5.LamAbs 2 (C5.LamApp (C5.LamApp (C5.LamVar 1) (C5.LamVar 2)) (C5.LamVar 0)))))))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_1" result expected

-- Test 2: Single macro definition in CPS
testChallenge5_2 :: IO ()
testChallenge5_2 = do
  let expr = C5.LamDef [("F", C5.LamAbs 1 (C5.LamVar 1))] (C5.LamVar 2)
      expected = C5.LamDef [("F", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 1 (C5.LamApp (C5.LamVar 1) (C5.LamVar 0))))))] (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamVar 2)))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_2" result expected

-- Test 3: Macro usage with application in CPS
testChallenge5_3 :: IO ()
testChallenge5_3 = do
  let expr = C5.LamDef [("F", C5.LamAbs 1 (C5.LamVar 1))] (C5.LamApp (C5.LamMacro "F") (C5.LamMacro "F"))
      expected = C5.LamDef [("F", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 1 (C5.LamApp (C5.LamVar 1) (C5.LamVar 0))))))] (C5.LamAbs 0 (C5.LamApp (C5.LamMacro "F") (C5.LamAbs 1 (C5.LamApp (C5.LamMacro "F") (C5.LamAbs 2 (C5.LamApp (C5.LamApp (C5.LamVar 1) (C5.LamVar 2)) (C5.LamVar 0)))))))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_3" result expected

-- Test 4: Complex macro with nested expressions in CPS
testChallenge5_4 :: IO ()
testChallenge5_4 = do
  let expr = C5.LamDef [("G", C5.LamAbs 1 (C5.LamApp (C5.LamVar 1) (C5.LamVar 1))), ("F", C5.LamAbs 1 (C5.LamApp (C5.LamMacro "G") (C5.LamVar 1)))] (C5.LamApp (C5.LamMacro "F") (C5.LamVar 2))
      expected = C5.LamDef [("G", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 1 (C5.LamApp (C5.LamApp (C5.LamVar 1) (C5.LamVar 1)) (C5.LamVar 0)))))), ("F", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 1 (C5.LamApp (C5.LamMacro "G") (C5.LamVar 1))))))] (C5.LamAbs 0 (C5.LamApp (C5.LamMacro "F") (C5.LamAbs 1 (C5.LamApp (C5.LamVar 2) (C5.LamVar 1)))))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_4" result expected

-- Test 5: CPS transformation of nested abstractions
testChallenge5_5 :: IO ()
testChallenge5_5 = do
  let expr = C5.LamDef [] (C5.LamAbs 1 (C5.LamAbs 2 (C5.LamApp (C5.LamVar 1) (C5.LamVar 2))))
      expected = C5.LamDef [] (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 1 (C5.LamAbs 2 (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamApp (C5.LamVar 1) (C5.LamVar 2))))))))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_5" result expected

-- Test 6: CPS transformation with multiple macros and nested usage
testChallenge5_6 :: IO ()
testChallenge5_6 = do
  let expr = C5.LamDef [("F", C5.LamAbs 1 (C5.LamApp (C5.LamVar 1) (C5.LamVar 1))), ("G", C5.LamAbs 2 (C5.LamApp (C5.LamVar 2) (C5.LamMacro "F")))] (C5.LamApp (C5.LamMacro "G") (C5.LamVar 3))
      expected = C5.LamDef [("F", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 1 (C5.LamApp (C5.LamApp (C5.LamVar 1) (C5.LamVar 1)) (C5.LamVar 0)))))), ("G", C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamAbs 0 (C5.LamAbs 2 (C5.LamApp (C5.LamApp (C5.LamVar 2) (C5.LamMacro "F")) (C5.LamVar 0))))))] (C5.LamAbs 0 (C5.LamApp (C5.LamMacro "G") (C5.LamAbs 3 (C5.LamApp (C5.LamVar 3) (C5.LamVar 0)))))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_6" result expected

-- Test 7: Edge case - empty macro definitions and variable usage
testChallenge5_7 :: IO ()
testChallenge5_7 = do
  let expr = C5.LamDef [] (C5.LamVar 0)
      expected = C5.LamDef [] (C5.LamAbs 0 (C5.LamApp (C5.LamVar 0) (C5.LamVar 0)))
      result = C5.cpsTransform expr
  checkTest "TestChallenge5_7" result expected


----------------------------------
------TESTS FOR CHALLENGE 6-------
----------------------------------

-- Test 1: Simple abstraction
testChallenge6_1 :: IO ()
testChallenge6_1 = do
  let expr = C6.LamDef [] (C6.LamAbs 1 (C6.LamApp (C6.LamVar 1) (C6.LamVar 2)))
      bound = 10
      expected = (Just 0, Just 0, Just 6, Just 6)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_1" result expected

-- Test 2: Macro usage
testChallenge6_2 :: IO ()
testChallenge6_2 = do
  let expr = C6.LamDef [("F", C6.LamAbs 1 (C6.LamVar 1))] (C6.LamMacro "F")
      bound = 10
      expected = (Just 1, Just 1, Just 3, Just 3)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_2" result expected

-- Test 3: Application of two abstractions
testChallenge6_3 :: IO ()
testChallenge6_3 = do
  let expr = C6.LamDef [] (C6.LamApp (C6.LamAbs 1 (C6.LamVar 1)) (C6.LamAbs 2 (C6.LamVar 2)))
      bound = 10
      expected = (Just 1, Just 1, Just 8, Just 8)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_3" result expected

-- Test 4: Non-terminating expression
testChallenge6_4 :: IO ()
testChallenge6_4 = do
  let expr = C6.LamDef [] (C6.LamApp (C6.LamAbs 1 (C6.LamApp (C6.LamVar 1) (C6.LamVar 1))) (C6.LamAbs 1 (C6.LamApp (C6.LamVar 1) (C6.LamVar 1))))
      bound = 100
      expected = (Nothing, Nothing, Nothing, Nothing)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_4" result expected

-- Test 5: Complex macros with application
testChallenge6_5 :: IO ()
testChallenge6_5 = do
  let expr = C6.LamDef 
               [("ID", C6.LamAbs 1 (C6.LamVar 1)), ("FST", C6.LamAbs 1 (C6.LamAbs 2 (C6.LamVar 1)))] 
               (C6.LamApp (C6.LamApp (C6.LamMacro "FST") (C6.LamVar 3)) (C6.LamApp (C6.LamMacro "ID") (C6.LamVar 4)))
      bound = 30
      expected = (Just 4, Just 4, Just 22, Just 22)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_5" result expected

-- Test 6: Macro and abstraction application
testChallenge6_6 :: IO ()
testChallenge6_6 = do
  let expr = C6.LamDef 
               [("FST", C6.LamAbs 1 (C6.LamAbs 2 (C6.LamVar 1)))] 
               (C6.LamApp (C6.LamApp (C6.LamMacro "FST") (C6.LamVar 3)) (C6.LamApp (C6.LamAbs 1 (C6.LamVar 1)) (C6.LamVar 4)))
      bound = 30
      expected = (Just 4, Just 3, Just 21, Just 21)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_6" result expected

-- Test 7: Nested non-terminating macro application
testChallenge6_7 :: IO ()
testChallenge6_7 = do
  let expr = C6.LamDef 
               [("ID", C6.LamAbs 1 (C6.LamVar 1)), 
                ("SND", C6.LamAbs 1 (C6.LamAbs 2 (C6.LamVar 2)))] 
               (C6.LamApp 
                 (C6.LamApp 
                   (C6.LamMacro "SND") 
                   (C6.LamApp 
                     (C6.LamAbs 1 (C6.LamApp (C6.LamVar 1) (C6.LamVar 1))) 
                     (C6.LamAbs 1 (C6.LamApp (C6.LamVar 1) (C6.LamVar 1)))
                   )
                 ) 
                 (C6.LamMacro "ID"))
      bound = 1000
      expected = (Nothing, Just 4, Nothing, Nothing)
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_7" result expected

-- Test 8: Simple macro with self-reference
testChallenge6_8 :: IO ()
testChallenge6_8 = do
  let expr = C6.LamDef [("SELF", C6.LamMacro "SELF")] (C6.LamMacro "SELF")
      bound = 10
      expected = (Nothing, Nothing, Nothing, Nothing) -- Self-referencing macro does not terminate
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_8" result expected

-- Test 9: Macro referencing another macro
testChallenge6_9 :: IO ()
testChallenge6_9 = do
  let expr = C6.LamDef [("F", C6.LamMacro "G"), ("G", C6.LamVar 1)] (C6.LamMacro "F")
      bound = 10
      expected = (Just 1, Just 1, Just 2, Just 2) -- Simple macro chain resolves quickly
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_9" result expected

-- Test 10: Application of a macro and a variable
testChallenge6_10 :: IO ()
testChallenge6_10 = do
  let expr = C6.LamDef [("ID", C6.LamAbs 1 (C6.LamVar 1))] (C6.LamApp (C6.LamMacro "ID") (C6.LamVar 2))
      bound = 10
      expected = (Just 1, Just 1, Just 4, Just 4) -- ID macro resolves and applies to the variable
      result = C6.compareInnerOuter expr bound
  checkTest "TestChallenge6_10" result expected

----------------------------------
------ MAIN FUNCTION -------------
----------------------------------
main :: IO ()
main = do
  putStrLn "Running tests for CHALLENGE 1..."
  test1
  test2
  test3
  test4
  test5
  test6
  putStrLn "Tests for Challenge1 completed!\n"

  putStrLn "Running tests for CHALLENGE 2..."
  testChallenge2_1
  testChallenge2_2
  testChallenge2_3
  testChallenge2_4
  testChallenge2_5
  testChallenge2_6
  putStrLn "Tests for Challenge2 completed!\n"

  putStrLn "Running tests for CHALLENGE 3..."
  testChallenge3_1
  testChallenge3_2
  testChallenge3_3
  testChallenge3_4
  testChallenge3_5
  testChallenge3_6
  testChallenge3_7
  testChallenge3_8
  putStrLn "Tests for Challenge3 completed!\n"

  putStrLn "Running tests for CHALLENGE 4..."
  testChallenge4_1
  testChallenge4_2
  testChallenge4_3
  testChallenge4_4
  testChallenge4_5
  testChallenge4_6
  testChallenge4_7
  testChallenge4_8
  testChallenge4_9
  testChallenge4_10
  putStrLn "Tests for Challenge4 completed!\n"


  putStrLn "Running tests for CHALLENGE 5..."
  testChallenge5_1
  testChallenge5_2
  testChallenge5_3
  testChallenge5_4
  testChallenge5_5
  testChallenge5_6
  testChallenge5_7
  putStrLn "Tests for Challenge5 completed!\n"

  putStrLn "Running tests for CHALLENGE 6..."
  testChallenge6_1
  testChallenge6_2
  testChallenge6_3
  testChallenge6_4
  testChallenge6_5
  testChallenge6_6
  testChallenge6_7
  testChallenge6_8
  testChallenge6_9
  testChallenge6_10
  putStrLn "Tests for Challenge6 completed!\n"
