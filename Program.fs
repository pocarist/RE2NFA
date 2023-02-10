let dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt

"a|b"
|> fun x -> dprintfn "%s" x; x
|> RE.parse 
|> RE.compile
|> dprintfn "%A"

"(ab|ac)d"
|> fun x -> dprintfn "%s" x; x
|> RE.parse 
|> RE.compile
|> dprintfn "%A"

"(ab|ac)d*e"
|> fun x -> dprintfn "%s" x; x
|> RE.parse 
|> RE.compile
|> dprintfn "%A"