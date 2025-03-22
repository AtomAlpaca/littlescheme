import Littlescheme.Basic
import Littlescheme.Eval
import Littlescheme.Parser
import Littlescheme.Tokenize
import Littlescheme.Print

#eval (Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (
  (Parse (Tokenize "(+ 1 1)")).fst
)).snd

#eval (Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (
  (Parse (Tokenize "((lambda (x) (+ x 1)) 1)")).fst
)).snd

#eval (Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (
  (Parse (Tokenize "(let x 1 (+ x x))")).fst
)).snd

#eval (Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (
  (Parse (Tokenize "(if true 0 1)")).fst
)).snd


partial def main : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← stdin.getLine
  stdout.putStrLn s!"{(Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty ((Parse (Tokenize input)).fst)).snd}"
