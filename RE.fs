module RE

type t =
    Chr of char
  | Star of t
  | Seq of t * t
  | Alt of t * t

(*
expression ::= 
          term
        | term '|' expression

term ::= 
        factor
      | factor term

factor ::=
        atom
      | atom '*'

atom ::=
        CHAR
      | '(' expression ')'

*)
type parser_context = {
  input: string
  pos: int
}

let init str = {
  input = str
  pos = 0
}

let rec expression pc =
  let t, pc = term pc
  if pc.pos < pc.input.Length && pc.input.[pc.pos] = '|' then
    let pc = {pc with pos = pc.pos+1}
    let t2, pc = expression pc
    Alt(t, t2), pc
  else
    t, pc
and term pc =
  let t, pc = factor pc
  if pc.pos < pc.input.Length && not ("|()*".Contains(pc.input.[pc.pos])) then
    let t2, pc = term pc
    Seq(t, t2), pc
  else
    t, pc
and factor pc =
  let t, pc = atom pc
  if pc.pos < pc.input.Length && pc.input.[pc.pos] = '*' then
    let pc = {pc with pos = pc.pos+1}
    Star(t), pc
  else
    t, pc
and atom pc =
  if pc.pos < pc.input.Length && pc.input.[pc.pos] = '(' then
    let pc = {pc with pos = pc.pos+1}
    let t, pc = expression pc
    let pc = {pc with pos = pc.pos+1} // ')'
    t, pc
  else
    let t = Chr(pc.input.[pc.pos])
    let pc = {pc with pos = pc.pos+1}
    t, pc

let parse str =
  init str
  |> expression
  |> fst

