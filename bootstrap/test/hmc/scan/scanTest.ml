open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Scan

let scan_str s =
  let rec fn t = begin
    let t', ctok = next t in
    let atok = ConcreteToken.atok ctok in
    let source = ConcreteToken.source ctok in
    File.Fmt.stdout
    |> Fmt.fmt "  "
    |> Source.Slice.pp source
    |> Fmt.fmt " : "
    |> AbstractToken.pp atok
    |> Fmt.fmt "\n"
    |> ignore;
    match atok with
    | Tok_end_of_input -> ()
    | _ -> fn t'
  end in
  File.Fmt.stdout
  |> String.fmt ~alt:true ~pretty:true s
  |> Fmt.fmt "\n"
  |> ignore;
  let t = init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t
