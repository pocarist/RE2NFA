let dprintfn fmt = Printf.ksprintf System.Diagnostics.Debug.WriteLine fmt

RE.parse "a|b"
|> dprintfn "%A"

RE.parse "(ab|ac)d"
|> dprintfn "%A"

RE.parse "(ab|ac)d*e"
|> dprintfn "%A"