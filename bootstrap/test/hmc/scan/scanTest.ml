open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Scan

let scan_str s =
  let rec fn t = begin
    let t', ctoken = next t in
    let atoken = ConcreteToken.atoken ctoken in
    let source = ConcreteToken.source ctoken in
    File.Fmt.stdout
    |> Fmt.fmt "  "
    |> Source.pp_loc source
    |> Fmt.fmt " : "
    |> AbstractToken.pp atoken
    |> Fmt.fmt "\n"
    |> ignore;
    match atoken with
    | Tok_end_of_input -> ()
    | _ -> fn t'
  end in
  File.Fmt.stdout
  |> String.fmt ~alt:true ~pretty:true s
  |> Fmt.fmt "\n"
  |> ignore;
  let t = init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t
