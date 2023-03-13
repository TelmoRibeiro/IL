module Expr where

-- abstract syntax for expressions
data Expr   = Num Int
            | Add Expr Expr
            | Sub Expr Expr -- 2)
            | Mul Expr Expr
            | Div Expr Expr -- 2)
            deriving (Eq, Show)

-- interpreter : a simple recursive denotational evaluator
eval :: Expr -> Int
eval (Num n) = n
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 + eval e2     -- 2)
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2 -- 2)

-- instructions for a stack machine
data Instr  = PUSH Int  -- push an integer onto the stack
            | ADD       -- add 2 values on top of the stack: push the result
            | SUB -- 2)
            | MUL       -- multiply 2 values on top of the stack; push the result
            | DIV -- 2)
            deriving (Eq, Show)

-- code is a list of instructions
type Code = [Instr]

-- compiler :
-- translator from expression to lists of instructions
compile :: Expr -> Code 
compile (Num n)     = [PUSH n]
compile (Add e1 e2) = compile e1 ++ compile e2 ++ [ADD]
compile (Sub e1 e2) = compile e1 ++ compile e2 ++ [SUB] -- 2)
compile (Mul e1 e2) = compile e1 ++ compile e2 ++ [MUL]
compile (Div e1 e2) = compile e1 ++ compile e2 ++ [DIV] -- 2)

-- vitual machine :
-- a stack is a list of value (integers)
type Stack = [Int]

-- the state of the machine: a pair of stack and code
type State = (Stack, Code, Int)

-- state transition; implements a single machine instruction
transition :: State -> State
transition (stack, PUSH n:code, size)     = (n:stack, code, size+1)           -- 3)
transition (v1:v2:stack, ADD:code, size)  = (v1+v2:stack, code, size-1)       -- 3)
transition (v1:v2:stack, SUB:code, size)  = (v1-v2:stack, code, size-1)       -- 2) & 3)
transition (v1:v2:stack, MUL:code, size)  = (v1*v2:stack, code, size-1)       -- 3)
transition (v1:v2:stack, DIV:code, size)  = (v1`div`v2:stack, code, size-1)   -- 2) & 3)
transition (_,_,_)                        = error "no valid transition"       -- 3)

-- execute a program starting with an empty stack
run :: Code -> Int
run code = runAux ([], code, 0)  -- 3)

-- worker function to execute instructions until the final state
-- the result is the value left on the top of the stack
runAux :: State -> Int
runAux s    | final s   = topStack s
            | otherwise = runAux (transition s)

-- state is final if code is empty
final :: State -> Bool
final (stack, code, size) = null code  -- 3)

-- get the value on the top of stack
topStack :: State -> Int
topStack (v:_,_,_) = v
topStack _        = error "empty stack"

-- 1)
repr :: Expr -> String
repr (Num n)     = show n
repr (Add e1 e2) = "(" ++ repr e1 ++ " + " ++ repr e2 ++ ")"
repr (Sub e1 e2) = "(" ++ repr e1 ++ " - " ++ repr e2 ++ ")" -- 2)
repr (Mul e1 e2) = "(" ++ repr e1 ++ " * " ++ repr e2 ++ ")"
repr (Div e1 e2) = "(" ++ repr e1 ++ " / " ++ repr e2 ++ ")" -- 2)

-- 2)
-- modified along the file

-- 3)
-- modified along the file

-- 4)
-- behave the same