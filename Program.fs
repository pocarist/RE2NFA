let dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt

let f re_str =
  re_str
  |> fun x -> printfn "\n%s" x; x
  |> RE.parse 
  |> fun x -> printfn "%A" x; x
  |> RE.compile
  |> fun nfa -> printfn "%A" nfa; nfa
  |> fun nfa ->
    // assert (RE.NFA.delta_transition nfa.delta 1 "a" = [2])
    // assert (RE.NFA.delta_D nfa.delta [0; 1; 3] ["a"] = [2; 5])
    // RE.NFA.epsilon_closure nfa.delta [0]
    // |> printfn "%A"
    // RE.NFA.delta_hat nfa.delta 1 ["a"]
    // |> printfn "%A"
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

  
    