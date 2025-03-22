import Littlescheme.Basic
import Init.Data.ToString
import Init.Data.ToString.Basic
import Init.Data.ToString.Macro
open Node

partial instance : ToString Node where
toString :=
let rec toStr x :=
  have : ToString Node := ⟨toStr⟩
  match x with
  | Ept => "null"
  | Num (value : Int) => s!"{value}"
  | Sym (name : String) => name
  | Bol (value : Bool) => s!"{value}"
  | Lst (list : List Node) => s!"({list})"
  | Lam (param : List Node) (body : Node) => s!"λ({param}).{body}"
  | If  (cond : Node) (fst : Node) (lst : Node) => s!"if {cond} then {fst} else {lst}"
  | Let (name : String) (value : Node) (body : Node) => s!"let {name} = {value}; {body}"
; toStr
