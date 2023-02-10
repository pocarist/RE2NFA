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

val parse: string -> t
val compile: t -> NFA