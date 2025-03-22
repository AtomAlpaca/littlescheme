import Littlescheme.Basic

mutual
partial def Eval
(builtinEnv : builtinEnvType)
(symbolEnv  : symbolEnvType)
(node : Node) : symbolEnvType × Node :=
  match node with
  | (Node.Sym x) => (symbolEnv,
      match (symbolEnv.get? x) with
      | (some y) => (Eval builtinEnv symbolEnv y).snd
      | none => node
    )
  | (Node.If cond x y) => (symbolEnv, EvalIf builtinEnv symbolEnv cond
                          (Eval builtinEnv symbolEnv x).snd (Eval builtinEnv symbolEnv y).snd)
  | (Node.Let x y body) => Eval builtinEnv (symbolEnv.insert x y) body
  | (Node.Lst x) => EvalList builtinEnv symbolEnv x
  | _ => (symbolEnv, node)

partial def SymbolEnvInsert
(symbolEnv  : symbolEnvType)
(names : List Node)
(values : List Node) : symbolEnvType :=
  match (names, values) with
  | (Node.Sym x :: _, y :: _) => symbolEnv.insert x y
  | (_, _) => symbolEnv

partial def EvalListHelper
(builtinEnv : builtinEnvType)
(symbolEnv  : symbolEnvType)
(list : List Node) : List Node :=
  match list with
  | [] => []
  | x :: xs => (Eval builtinEnv symbolEnv x).snd :: EvalListHelper builtinEnv symbolEnv xs

partial def EvalList
(builtinEnv : builtinEnvType)
(symbolEnv  : symbolEnvType)
(list : List Node) : symbolEnvType × Node :=
  match list with
  | Node.Sym x :: _rest =>
    let rest := EvalListHelper builtinEnv symbolEnv _rest;
    match builtinEnv.get? x with
    | some f => (symbolEnv, f rest)
    | none => match symbolEnv.get? x with
      | some (Node.Lam param body) => EvalLambda builtinEnv symbolEnv (Node.Lam param body) rest
      | _ => (symbolEnv, Node.Lst list)
  | (Node.Lam param body) :: rest => EvalLambda builtinEnv symbolEnv (Node.Lam param body) rest
  | _ => (symbolEnv, Node.Lst list)

partial def EvalLambda
(builtinEnv : builtinEnvType)
(symbolEnv  : symbolEnvType)
(lam : Node)
(args : List Node) : symbolEnvType × Node :=
  match lam with
  | Node.Lam params body =>
    if params.length = args.length then
      Eval builtinEnv (SymbolEnvInsert symbolEnv params args) body
    else
      (symbolEnv, Node.Ept)
  | _ => (symbolEnv, Node.Ept)


partial def EvalIf
(builtinEnv : builtinEnvType)
(symbolEnv  : symbolEnvType)
(cond : Node)
(x y : Node) : Node :=
  match Eval builtinEnv symbolEnv cond with
  | (_, Node.Bol false) => y
  | _ => x
end

-- TODO: Prove It!

#eval (Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (Node.Lst [Node.Sym "+", Node.Num 1, Node.Num 2])).snd
