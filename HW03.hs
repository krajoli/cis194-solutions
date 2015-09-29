module HW03 where

data Expression =
    Var String                   -- Variable
  | Val Int                      -- Integer literal
  | Op Expression Bop Expression -- Operation
  deriving (Show, Eq)

-- Binary (2-input) operators
data Bop = 
    Plus     
  | Minus    
  | Times    
  | Divide   
  | Gt
  | Ge       
  | Lt  
  | Le
  | Eql
  deriving (Show, Eq)

data Statement =
    Assign   String     Expression
  | Incr     String
  | If       Expression Statement  Statement
  | While    Expression Statement       
  | For      Statement  Expression Statement Statement
  | Sequence Statement  Statement        
  | Skip
  deriving (Show, Eq)

type State = String -> Int

-- Exercise 1 -----------------------------------------

extend :: State -> String -> Int -> State
extend st s i = \k -> if (k == s) then i else st k

empty :: State
empty _ = 0

-- Exercise 2 -----------------------------------------

evalE :: State -> Expression -> Int
evalE st (Val v) = v
evalE st (Var s) = st s
evalE st (Op e1 bop e2)
      | bop == Plus = s1 + s2
      | bop == Minus = s1 - s2
      | bop == Times = s1 * s2
      | bop == Divide = s1 `div` s2
      | bop == Gt = if (s1 > s2) then 1 else 0
      | bop == Ge = if (s1 >= s2) then 1 else 0
      | bop == Lt = if (s1 < s2) then 1 else 0
      | bop == Le = if (s1 <= s2) then 1 else 0
      | bop == Eql = if (s1 == s2) then 1 else 0
      | otherwise = 0
      where s1 = evalE st e1
            s2 = evalE st e2

-- Exercise 3 -----------------------------------------

data DietStatement = DAssign String Expression
                   | DIf Expression DietStatement DietStatement
                   | DWhile Expression DietStatement
                   | DSequence DietStatement DietStatement
                   | DSkip
                     deriving (Show, Eq)

desugar :: Statement -> DietStatement
desugar (Assign s expr) = DAssign s expr
desugar (If testExpr s1 s2) = DIf testExpr (desugar s1) (desugar s2)
desugar (Incr s) = DAssign s (Op (Var s) Plus (Val 1))
desugar (While testExpr s1) = DWhile testExpr (desugar s1)
desugar (For init testExpr update body) = (DSequence (desugar init) $ DWhile testExpr (DSequence (desugar body) (desugar update)))
desugar (Sequence s1 s2) = DSequence (desugar s1) (desugar s2)
desugar s = DSkip


-- Exercise 4 -----------------------------------------

evalSimple :: State -> DietStatement -> State
evalSimple st (DAssign s e) = extend st s (evalE st e)
evalSimple st (DIf testExpr trueStat falseStat) = if (evalE st testExpr == 1) then (evalSimple st trueStat) else (evalSimple st falseStat)
evalSimple st (DWhile testExpr bodyStat) = f st testExpr bodyStat
           where f inState te bs
                   | (evalE inState te) == 0 = inState
                   | (evalE inState te) == 1 = f (evalSimple inState bs) te bs
evalSimple st (DSequence ds1 ds2) = evalSimple (evalSimple st ds1) ds2
evalSimple st anyOther = st

run :: State -> Statement -> State
run st statmt = evalSimple st $ desugar statmt

-- Programs -------------------------------------------

slist :: [Statement] -> Statement
slist [] = Skip
slist l  = foldr1 Sequence l

{- Calculate the factorial of the input

   for (Out := 1; In > 0; In := In - 1) {
     Out := In * Out
   }
-}
factorial :: Statement
factorial = For (Assign "Out" (Val 1))
                (Op (Var "In") Gt (Val 0))
                (Assign "In" (Op (Var "In") Minus (Val 1)))
                (Assign "Out" (Op (Var "In") Times (Var "Out")))


{- Calculate the floor of the square root of the input

   B := 0;
   while (A >= B * B) {
     B++
   };
   B := B - 1
-}
squareRoot :: Statement
squareRoot = slist [ Assign "B" (Val 0)
                   , While (Op (Var "A") Ge (Op (Var "B") Times (Var "B")))
                       (Incr "B")
                   , Assign "B" (Op (Var "B") Minus (Val 1))
                   ]

{- Calculate the nth Fibonacci number

   F0 := 1;
   F1 := 1;
   if (In == 0) {
     Out := F0
   } else {
     if (In == 1) {
       Out := F1
     } else {
       for (C := 2; C <= In; C++) {
         T  := F0 + F1;
         F0 := F1;
         F1 := T;
         Out := T
       }
     }
   }
-}
fibonacci :: Statement
fibonacci = slist [ Assign "F0" (Val 1)
                  , Assign "F1" (Val 1)
                  , If (Op (Var "In") Eql (Val 0))
                       (Assign "Out" (Var "F0"))
                       (If (Op (Var "In") Eql (Val 1))
                           (Assign "Out" (Var "F1"))
                           (For (Assign "C" (Val 2))
                                (Op (Var "C") Le (Var "In"))
                                (Incr "C")
                                (slist
                                 [ Assign "T" (Op (Var "F0") Plus (Var "F1"))
                                 , Assign "F0" (Var "F1")
                                 , Assign "F1" (Var "T")
                                 , Assign "Out" (Var "T")
                                 ])
                           )
                       )
                  ]
