open Rudiments_int0
open Rudiments_functions

(* On success this function returns an array of entropy bits, where the total
 * number of bits is rounded up frome the number of bits requested to the
 * nearest multiple of 64. *)
external entropy_nbits: usize -> Int64.t array = "hemlock_entropy_nbits"

let get () =
  match entropy_nbits 128 with
  | [|hi; lo|] -> {hi; lo}
  | _ -> halt "Entropy.get error: Entropy acquisition failure"

let seed =
  match Sys.getenv_opt "HEMLOCK_ENTROPY" with
  | None -> get ()
  | Some hemlock_entropy -> u128_of_string hemlock_entropy
