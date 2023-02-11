let dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt

let f re_str =
  re_str
  |> fun x -> printfn "%s" x; x
  |> RE.parse 
  |> RE.compile
  |> fun nfa -> printfn "%A" nfa; nfa
  |> fun nfa ->
    RE.NFA.epsilon_closure nfa.delta [0]
    |> printfn "%A"
    RE.NFA.delta_hat nfa.delta 3 ["a"]
    |> printfn "%A"
    RE.nfa2dfa nfa
    |> printfn "%A"

"a|b"
|> f

"(ab|ac)d"
|> f

"(ab|ac)d*e"
|> f

  
    