module RE

type S = string
type Q = int
type delta = (Q * (S * Q list) list) list
type NFA = {
    Q: Q list
    S: S list
    delta: delta
    q0: Q
    F: Q list
}

type state = Q list
type Delta = (state * (S * state) list) list
type DFA = {
    Q: state list
    S: S list
    Delta: Delta
    Q0: state
    F: state list
}

type t =
  | Empty
  | Chr of char
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

module Parser =
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
  Parser.init str
  |> Parser.expression
  |> fst

module Compiler =
  let counter = ref 0
  let gen_p () =
    let p = !counter
    incr counter
    p

  let rec f = function
    | Empty -> 
      let p = gen_p ()
      let q = gen_p ()
      {
        Q = [p; q]
        S = []
        delta = []
        q0 = p
        F = [q]
      }
    | Chr c -> 
      let p = gen_p ()
      let q = gen_p ()
      let str = c.ToString()
      {
        Q = [p; q]
        S = [str]
        delta = [p, [(str, [q])]]
        q0 = p
        F = [q]
      }
    | Star r1 -> 
      let p = gen_p ()
      let n1 = f r1
      let q = gen_p ()
      {
        Q = n1.Q @ [p; q]
        S = n1.S
        delta = n1.delta @ [
                            p, ["", [n1.q0; q]]
                            n1.F.Head, ["", [n1.q0; q]]
                          ]
        q0 = p
        F = [q]
      }
    | Seq(r1, r2) -> 
      let p = gen_p ()
      let n1 = f r1
      let n2 = f r2
      let q = gen_p ()
      {
        Q = n1.Q @ n2.Q @ [p; q]
        S = n1.S @ n2.S
        delta = n1.delta @ n2.delta @ [
                                        p, ["", [n1.q0]]
                                        n1.F.Head, ["", [n2.q0]]
                                        n2.F.Head, ["", [q]]
                                      ]
        q0 = p
        F = [q]
      }
    | Alt(r1, r2) -> 
      let p = gen_p ()
      let n1 = f r1
      let n2 = f r2
      let q = gen_p ()
      {
        Q = n1.Q @ n2.Q @ [p; q]
        S = n1.S @ n2.S
        delta = n1.delta @ n2.delta @ [
                                        p, ["", [n1.q0; n2.q0]]
                                        n1.F.Head, ["", [q]]
                                        n2.F.Head, ["", [q]]
                                      ]
        q0 = p
        F = [q]
      }

let compile t =
  Compiler.counter := 0
  let nfa = Compiler.f t
  {
    nfa with
      Q = nfa.Q |> Set.ofList |> Set.toList
      S = nfa.S |> Set.ofList |> Set.toList
      delta = nfa.delta |> Set.ofList |> Set.toList
  }

module NFA =
  (*
type delta = (Q * (S * Q list) list) list
type NFA = {
    Q: Q list
    S: S list
    delta: delta
    q0: Q
    F: Q list
}

  *)

  // delta transition
  let delta_transition (delta:delta) p a =
    match List.tryFind (fun (x, _) -> p = x) delta with
    | Some (_, sqs) ->
      sqs
      |> List.map (fun (s, qs) -> if s = a then qs else [])
      |> List.concat            
    | None -> []

  // epsilon closure
  let epsilon_closure (delta:delta) P =
    let rec loop visited acc = function 
      | [] -> acc |> Set.ofList |> Set.toList
      | p :: xs ->
        if Set.contains p visited then
          loop visited acc xs
        else
          let visited = Set.add p visited
          let ys =
            delta_transition delta p ""
          let acc = p :: acc
          loop visited acc (xs@ys)
    loop Set.empty [] P

  // delta hat
  let rec delta_hat delta p = function
    | [] -> epsilon_closure delta [p]
    | a :: w ->
      delta_hat delta p w
      |> List.map (fun q -> delta_transition delta q a)
      |> List.concat
      |> Set.ofList
      |> Set.toList
      // |> epsilon_closure delta
      
  // let delta_hat (delta:delta) p (wa:S list) = 
  //   let rec loop (p:Q) = function
  //     | [] -> epsilon_closure delta [p]
  //     | a :: w -> 
  //       loop p w
  //       |> List.map (fun q -> 
  //         delta_transition delta q a
  //       )
  //       |> List.concat
  //       |> Set.ofList
  //       |> Set.toList
  //   loop p wa
  //   |> epsilon_closure delta

  // delta_D
  let delta_D delta P a =
    P
    |> List.map (fun p ->
      delta_hat delta p [a]
    )
    |> List.concat
    |> Set.ofList
    |> Set.toList

module DFA =
(*
type state = Q list
type Delta = (state * (S * state) list) list
type DFA = {
    Q: state list
    S: S list
    Delta: Delta
    Q0: state
    F: state list
}
*)
  let toDFA nfa =
    let Cl = NFA.epsilon_closure nfa.delta
    let delta_hat = NFA.delta_hat nfa.delta
    let delta_D = NFA.delta_D nfa.delta
    let addS (A, s) (Q1, Q2, Omega) =
      let A' = delta_D A s
      let Q1' =
        if List.contains A' ([A] @ Q1 @ Q2) then Q1
        else [A'] @ Q1
      Q1', [(s, A')] @ Omega
    let addQ A (Q1, Q2, Delta) =
      let (q1, omega) =
        nfa.S
        |> List.fold (fun (q1, omega) s -> 
          addS (A, s) (q1, Q2, omega)
        ) (Q1, [])
      Q1, [A] @ Q2, [A, omega] @ Delta
    let rec subsets = function
      | [], Q2, Delta -> Q2, Delta
      | (A: Q list) :: Q1, (Q2: Q list list), (Delta:Delta) -> subsets (addQ A (Q1, Q2, Delta))

    let A = Cl [nfa.q0]
    let (Q, Delta) = subsets ([A], [], [])
    let Fin =
      let Fs = nfa.F |> Set.ofList
      Q
      |> List.filter (fun q ->
        let qs = Set.ofList q
        Set.intersect qs Fs <> Set.empty
      )
    {
      DFA.Q = Q
      S = nfa.S
      Delta = Delta
      Q0 = A
      F = Fin
    }
      
let nfa2dfa nfa =
  DFA.toDFA nfa
  