open RudimentsInt0
open RudimentsFunctions

(* On success this function returns an array of entropy bits, where the total number of bits is
 * rounded up frome the number of bits requested to the nearest multiple of 64. *)
external entropy_nbits: uns -> Int64.t array = "hemlock_entropy_nbits"

let get () =
  match entropy_nbits 128 with
  | [|lo; hi|] -> u128_of_tup (lo, hi)
  | _ -> halt "Entropy.get error: Entropy acquisition failure"

let seed =
  match Sys.getenv_opt "HEMLOCK_ENTROPY" with
  | None -> get ()
  | Some hemlock_entropy -> u128_of_string hemlock_entropy
