def IsValidChar (c : Char) : Bool := c != ' ' && c != '(' && c != ')'

partial def Tokenize (str : String) : List String :=
  match str with
  | "" => []
  | _ => match str.get 0 with
    | '(' => "(" :: Tokenize (str.drop 1)
    | ')' => ")" :: Tokenize (str.drop 1)
    | ' ' => Tokenize (str.drop 1)
    | _ => str.takeWhile IsValidChar :: Tokenize (str.dropWhile IsValidChar)

#eval Tokenize "(n + 2 1 3 4)"
