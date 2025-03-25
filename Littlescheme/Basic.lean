import Std.Data.HashMap


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

def NodeQuote (xs : List Node) : Node :=
  match xs.get? 0 with
  | none => Node.Ept
  | some x => x

def NodeAtom (xs : List Node) : Node :=
  match xs.get? 0 with
  | none => Node.Ept
  | some x => match x with
    | Node.Num _ => Node.Bol true
    | Node.Sym _ => Node.Bol true
    | Node.Bol _ => Node.Bol true
    | Node.Lst [] => Node.Bol true
    | _ => Node.Lst []

def NodeCar (xs : List Node) : Node :=
  match xs.get? 0 with
  | some (Node.Lst (x :: _)) => x
  | _ => Node.Ept

def NodeCdr (xs : List Node) : Node :=
  match xs.get? 0 with
  | some (Node.Lst (_ :: x)) => Node.Lst x
  | _ => Node.Ept

def NodeCons (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | (some x, some (Node.Lst y)) => Node.Lst (x :: y)
  | _ => Node.Ept

def NodeCond (xs : List Node) : Node :=
  match xs with
  | [] => Node.Ept
  | Node.Lst (Node.Bol true :: y :: _) :: _ => y
  | _ :: rest => NodeCond rest

mutual
partial def NodeEqHelperList (xs : List Node) (ys : List Node) : Bool :=
  match (xs, ys) with
  | ([], _) => true
  | (_, []) => true
  | (x :: restx, y :: resty) => match NodeEqHelper x y with
    | false => false
    | true => match NodeEqHelperList restx resty with
      | false => false
      | true => true

partial def NodeEqHelper (x : Node) (y : Node) : Bool :=
  match (x, y) with
  | (Node.Ept, Node.Ept) => true
  | (Node.Num x, Node.Num y) => x = y
  | (Node.Sym x, Node.Sym y) => x = y
  | (Node.Bol x, Node.Bol y) => x = y
  | (Node.Lst x, Node.Lst y) => NodeEqHelperList x y
  | (Node.Lam paramx bodyx, Node.Lam paramy bodyy) => NodeEqHelperList paramx paramy && NodeEqHelper bodyx bodyy
  | (Node.If condx fstx sndx, Node.If condy fsty sndy) => NodeEqHelper condx condy && NodeEqHelper fstx fsty && NodeEqHelper sndx sndy
  | (Node.Let namex valuex bodyx, Node.Let namey valuey bodyy) => namex = namey && NodeEqHelper valuex valuey && NodeEqHelper bodyx bodyy
  | _ => false

partial def NodeEq (xs : List Node) : Node :=
  match ((xs.get? 0), (xs.get? 1)) with
  | (some x, some y) => Node.Bol (NodeEqHelper x y)
  | _ => Node.Bol false
end
def builtinSymbols : List (String × (List Node → Node)) :=
[
  ("+", NodeAdd),
  ("-", NodeMinus),
  ("<",  NodeLess),
  ("<=", NodeLessEq),
  (">",  NodeGreater),
  (">=", NodeGreaterEq),
  ("quote", NodeQuote),
  ("\'", NodeQuote),
  ("atom", NodeAtom),
  ("car", NodeCar),
  ("cdr", NodeCdr),
  ("cons", NodeCons),
  ("cond", NodeCond),
  ("eq", NodeEq)
]
def getSymbolEnv
(xs : List (String × (List Node → Node)))
: builtinEnvType :=
  match xs with
  | [] => Std.HashMap.empty
  | (x, y) :: rest => (getSymbolEnv rest).insert x y
