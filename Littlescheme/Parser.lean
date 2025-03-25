import Littlescheme.Basic
mutual
partial def Parse
(list : List String) : Node × List String :=
  match list with
  | "(" :: rest => PraseSeq rest
  | fst :: rest => (PraseAtom fst, rest)
  | _ => (Node.Ept, [])

partial def PraseSeq
(list : List String) : Node × List String :=
  match list.head! with
  | "lambda" => PraseLam list
  | "let"    => PraseLet list
  | "if"     => PraseIf  list
  | ")"      => (Node.Lst [], list.tail)
  | _        => let (fst, snd) := PraseList list; (Node.Lst fst, snd)

partial def PraseList
(list : List String) : List Node × List String :=
  if list.isEmpty then ([], [])
  else match list with
  | ")" :: rest => ([], rest)
  | _ => let (res1, rest1) := Parse list; let (res2, rest2) := PraseList rest1; (res1 :: res2, rest2)

partial def PraseLam
(list : List String) : Node × List String :=
  let (result, rest) := PraseList list;
  match ((result.get? 1), (result.get? 2)) with
    | (some (Node.Lst x), some y) => (Node.Lam x y, rest)
    | _ => (Node.Ept, rest)

partial def PraseIf
(list : List String) : Node × List String :=
  let (result, rest) := PraseList list;
  match ((result.get? 1), (result.get? 2), (result.get? 3)) with
    | (some cond, some x, some y) => (Node.If cond x y, rest)
    | _ => (Node.Ept, rest)

partial def PraseLet
(list : List String) : Node × List String :=
  let (result, rest) := PraseList list;
  match ((result.get? 1), (result.get? 2), (result.get? 3)) with
    | (some (Node.Sym x), some y, some body) => (Node.Let x y body, rest)
    | _ => (Node.Ept, rest)

partial def PraseAtom
(str : String) : Node :=
  match str with
  | "true"  => Node.Bol true
  | "false" => Node.Bol false
  | "null"  => Node.Ept
  | _ => match str.toNat? with
    | some x => Node.Num x
    | _ => Node.Sym str

end
