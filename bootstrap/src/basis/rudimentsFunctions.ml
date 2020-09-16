let not_reached () =
  let () = prerr_string "Unreachable code reached\n" in
  assert false

let not_implemented s =
  let () = prerr_string ("Not implemented: " ^ s ^ "\n") in
  assert false

let halt s =
  let () = prerr_string ("Halt: " ^ s ^ "\n") in
  exit 1

let demand e =
  assert e

let not t =
  match t with
  | false -> true
  | true -> false
