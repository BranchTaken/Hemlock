open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let rec fn i = begin
    match i with
    | 0x80 -> ()
    | _ -> begin
        printf "%a -> \"%s\"\n"
          Uns.pp_x i (escaped (of_codepoint Codepoint.(of_uns i)));
        fn (Uns.succ i)
      end
  end in
  fn 0

let _ = test ()
