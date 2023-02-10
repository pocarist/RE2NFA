module RE

type t =
    Chr of char
  | Star of t
  | Seq of t * t
  | Alt of t * t

val parse: string -> t