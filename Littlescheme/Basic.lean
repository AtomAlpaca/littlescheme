import Std.Data.HashMap

def hello := "world"

inductive Node where
  | Ept
  | Num (value : Int)
  | Sym (name : String)
  | Bol (value : Bool)
  | Lst (list : List Node)
  | Lam (param : List Node) (body : Node)
  | If  (cond : Node) (fst : Node) (lst : Node)
  | Let (name : String) (value : Node) (body : Node)
deriving Nonempty

def builtinEnvType := Std.HashMap String (List Node → Node)
def symbolEnvType  := Std.HashMap String Node

def convertNodeListToNumList (xs : List Node) : List Int :=
  match xs with
  | (Node.Num x) :: rest => x :: convertNodeListToNumList rest
  | _ => []

def NodeAdd (xs : List Node) : Node :=
  Node.Num ((convertNodeListToNumList xs).foldl (. + .) 0)

#eval NodeAdd [(Node.Num 1), (Node.Num 2), (Node.Num 3)]

def NodeMinus (xs : List Node) : Node :=
  match xs with
  | (Node.Num x) :: rest => Node.Num (x - ((convertNodeListToNumList rest).foldl (. + .) 0))
  | _ => Node.Num 0

#eval NodeMinus [(Node.Num 10), (Node.Num 2), (Node.Num 3)]

def NodeLess (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | ((Node.Num x), (Node.Num y))  => Node.Bol (x < y)
  | _ => Node.Ept

def NodeLessEq (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | ((Node.Num x), (Node.Num y))  => Node.Bol (x <= y)
  | _ => Node.Ept

def NodeGreater (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | ((Node.Num x), (Node.Num y))  => Node.Bol (x > y)
  | _ => Node.Ept

def NodeGreaterEq (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | ((Node.Num x), (Node.Num y))  => Node.Bol (x >= y)
  | _ => Node.Ept

def builtinSymbols : List (String × (List Node → Node)) :=
[
  ("+", NodeAdd),
  ("-", NodeMinus),
  ("<",  NodeLess),
  ("<=", NodeLessEq),
  (">",  NodeGreater),
  (">=", NodeGreaterEq),
]

def getSymbolEnv
(xs : List (String × (List Node → Node)))
: builtinEnvType :=
  match xs with
  | [] => Std.HashMap.empty
  | (x, y) :: rest => (getSymbolEnv rest).insert x y
