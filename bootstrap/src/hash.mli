type t = int
type state

val state_of_int: int -> state
val t_of_state: state -> t

val hash_fold: state -> 'a -> state
val hash: 'a -> t
