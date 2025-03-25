import Littlescheme.Basic
import Littlescheme.Eval
import Littlescheme.Parser
import Littlescheme.Tokenize
import Littlescheme.Print

def EvalHelper (str : String) : Node :=
(Eval (getSymbolEnv builtinSymbols) Std.HashMap.empty (
  (Parse (Tokenize str)).fst
)).snd

#eval EvalHelper "(+ 1 1)"
#eval EvalHelper "(+ (+ 1 1) (+ 1 1))"
#eval EvalHelper "((lambda (x) (+ x 1)) 1)"
#eval EvalHelper "(let x 1 (+ x x))"
#eval EvalHelper "(if true 0 1)"

#eval EvalHelper "(quote a)"
#eval EvalHelper "(quote (+ 1 1))"
#eval EvalHelper "(eq a a)"
#eval EvalHelper "()"
#eval EvalHelper "(cons a (b c))"
#eval EvalHelper "(cons a (cons b (cons c ())))"
#eval EvalHelper "(cons a (b c))"
#eval EvalHelper "(atom a)"
#eval EvalHelper "(car (cons a (b c)))"
#eval EvalHelper "(cdr (cons a (b c)))"
#eval EvalHelper "(cond ((eq 'a 'b) 'first) ((atom 'a)  'second))"


partial def main : IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← stdin.getLine
  stdout.putStrLn s!"{EvalHelper input}"
