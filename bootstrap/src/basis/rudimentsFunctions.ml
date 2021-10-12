let print_error msg =
  let () = prerr_string (msg ^ "\nat:\n") in
  prerr_string Printexc.(raw_backtrace_to_string (get_callstack 64))

let not_reached () =
  let () = print_error "Unreachable code reached" in
  assert false

let not_implemented s =
  let () = print_error ("Not implemented: " ^ s) in
  assert false

let halt s =
  let () = print_error ("Halt: " ^ s) in
  exit 1

let not t =
  match t with
  | false -> true
  | true -> false
