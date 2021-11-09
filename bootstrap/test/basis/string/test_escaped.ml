open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let rec fn i = begin
    match i with
    | 0x80L -> ()
    | _ -> begin
        printf "%a -> \"%s\"\n"
          Uns.xpp_x i (escaped (of_codepoint Codepoint.(trunc_of_uns i)));
        fn (Uns.succ i)
      end
  end in
  fn 0L

let _ = test ()
