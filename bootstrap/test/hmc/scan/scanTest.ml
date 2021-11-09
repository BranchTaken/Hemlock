open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Scan

let scan_str s =
  let open Format in
  let rec fn t = begin
    let t', ctoken = next t in
    let atoken = ConcreteToken.atoken ctoken in
    let source = ConcreteToken.source ctoken in
    printf "  %a : %s\n"
      Source.xpp_loc source
      (AbstractToken.to_string atoken)
    ;
    match atoken with
    | Tok_end_of_input -> ()
    | _ -> fn t'
  end in
  printf "{|%s|}\n" s;
  let t = init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t
