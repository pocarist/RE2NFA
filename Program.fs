open CommandLine

type options = {
  [<Option('r', "re", HelpText = "show regular expression.")>] show_re: bool
  [<Option('n', "nfa", HelpText = "show NFA.")>] show_nfa: bool
  [<Option('d', "dfa", HelpText = "show DFA.")>] show_dfa: bool
  [<Option('N', "nfa-diagram", HelpText = "show NFA state diagram.")>] show_nfa_diagram: bool
  [<Option('D', "dfa-diagram", HelpText = "show DFA state diagram.")>] show_dfa_diagram: bool
  [<Value(0, Required = true, MetaName = "regexp", HelpText = "regexp pattern.")>] regexp : string
  [<Value(1, MetaName = "accept", HelpText = "check accept string.")>] accept: string option
}

let run o =
  let re = RE.parse o.regexp

  if o.show_re then
    re
    |> printfn "%A"

  let nfa = RE.compile re
  if o.show_nfa then
    nfa
    |> printfn "%A"

  let dfa = RE.nfa2dfa nfa
  if o.show_dfa then
    dfa
    |> printfn "%A"

  if o.show_nfa_diagram then
    RE.nfa_to_state_diagram nfa
    |> printfn "%s"

  if o.show_dfa_diagram then
    RE.dfa_to_state_diagram dfa
    |> printfn "%s"

  match o.accept with
  | Some s ->
    printfn "regexp: %s, input: %s, Accept: %s" o.regexp s (if RE.accept dfa s then "OK" else "NG")
  | _ -> ()

  0
      
let fail _ =
  -1

[<EntryPoint>]
let main argv =
  let result = CommandLine.Parser.Default.ParseArguments<options>(argv)
  match result with
  | :? Parsed<options> as parsed -> run parsed.Value
  | :? NotParsed<options> as notParsed -> fail notParsed.Errors  

// f "a|b" ["a", true; "ab", false] 
// f "(a|b)*ab(a|b)*c" ["aabbbabc", true; "aacbbbabc", false]

(*
-----------
Input: a|b
RE: Alt (Chr 'a', Chr 'b')
NFA: { Q = [0; 1; 2; 3; 4; 5]
  S = ["a"; "b"]
  delta =
   [(0, [("", [1; 3])]); (1, [("a", [2])]); (2, [("", [5])]); (3, [("b", [4])]);
    (4, [("", [5])])]
  q0 = 0
  F = [5] }
DFA: { Q = [[2; 5]; []; [4; 5]; [0; 1; 3]]
  S = ["a"; "b"]
  Delta =
   [([2; 5], [("b", []); ("a", [])]); ([], [("b", []); ("a", [])]);
    ([4; 5], [("b", []); ("a", [])]);
    ([0; 1; 3], [("b", [4; 5]); ("a", [2; 5])])]
  Q0 = [0; 1; 3]
  F = [[2; 5]; [4; 5]] }
Accept: a -> OK
Accept: ab -> NG
-----------
Input: (a|b)*ab(a|b)*c
RE: Seq
  (Star (Alt (Chr 'a', Chr 'b')),
   Seq (Chr 'a', Seq (Chr 'b', Seq (Star (Alt (Chr 'a', Chr 'b')), Chr 'c'))))
NFA: { Q =
   [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20; 21;
    22; 23; 24; 25; 26; 27; 28; 29]
  S = ["a"; "b"; "c"]
  delta =
   [(0, [("", [1])]); (1, [("", [2; 8])]); (2, [("", [3; 5])]);
    (3, [("a", [4])]); (4, [("", [7])]); (5, [("b", [6])]); (6, [("", [7])]);
    (7, [("", [2; 8])]); (8, [("", [9])]); (9, [("", [10])]);
    (10, [("a", [11])]); (11, [("", [12])]); (12, [("", [13])]);
    (13, [("b", [14])]); (14, [("", [15])]); (15, [("", [16])]);
    (16, [("", [17; 23])]); (17, [("", [18; 20])]); (18, [("a", [19])]);
    (19, [("", [22])]); (20, [("b", [21])]); (21, [("", [22])]);
    (22, [("", [17; 23])]); (23, [("", [24])]); (24, [("c", [25])]);
    (25, [("", [26])]); (26, [("", [27])]); (27, [("", [28])]);
    (28, [("", [29])])]
  q0 = 0
  F = [29] }
DFA: { Q =
   [[2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 21; 22; 23; 24];
    [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24];
    [2; 3; 5; 6; 7; 8; 9; 10; 17; 18; 20; 21; 22; 23; 24]; [25; 26; 27; 28; 29];
    [2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 23; 24];
    [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13]; [2; 3; 5; 6; 7; 8; 9; 10]; [];
    [0; 1; 2; 3; 5; 8; 9; 10]]
  S = ["a"; "b"; "c"]
  Delta =
   [([2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 21; 22; 23; 24],
     [("c", [25; 26; 27; 28; 29]);
      ("b", [2; 3; 5; 6; 7; 8; 9; 10; 17; 18; 20; 21; 22; 23; 24]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24])]);
    ([2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24],
     [("c", [25; 26; 27; 28; 29]);
      ("b", [2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 21; 22; 23; 24]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24])]);
    ([2; 3; 5; 6; 7; 8; 9; 10; 17; 18; 20; 21; 22; 23; 24],
     [("c", [25; 26; 27; 28; 29]);
      ("b", [2; 3; 5; 6; 7; 8; 9; 10; 17; 18; 20; 21; 22; 23; 24]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24])]);
    ([25; 26; 27; 28; 29], [("c", []); ("b", []); ("a", [])]);
    ([2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 23; 24],
     [("c", [25; 26; 27; 28; 29]);
      ("b", [2; 3; 5; 6; 7; 8; 9; 10; 17; 18; 20; 21; 22; 23; 24]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13; 17; 18; 19; 20; 22; 23; 24])]);
    ([2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13],
     [("c", []);
      ("b", [2; 3; 5; 6; 7; 8; 9; 10; 14; 15; 16; 17; 18; 20; 23; 24]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13])]);
    ([2; 3; 5; 6; 7; 8; 9; 10],
     [("c", []); ("b", [2; 3; 5; 6; 7; 8; 9; 10]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13])]);
    ([], [("c", []); ("b", []); ("a", [])]);
    ([0; 1; 2; 3; 5; 8; 9; 10],
     [("c", []); ("b", [2; 3; 5; 6; 7; 8; 9; 10]);
      ("a", [2; 3; 4; 5; 7; 8; 9; 10; 11; 12; 13])])]
  Q0 = [0; 1; 2; 3; 5; 8; 9; 10]
  F = [[25; 26; 27; 28; 29]] }
Accept: aabbbabc -> OK
Accept: aacbbbabc -> NG
*)