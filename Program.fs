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
    RE.nfa2dfa nfa
    |> printfn "%A"

"a|b"
|> f

// [$ (a|b)^\ast ab(a|b)^\ast c]
"(a|b)*ab(a|b)*c"
|> f

// "(ab|ac)d"
// |> f

// "(ab|ac)d*e"
// |> f

  
    