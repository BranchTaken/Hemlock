open! Basis.Rudiments
open! Basis
open! Hmc
open Hmc.Scan

let scan_str s =
  let rec fn t = begin
    let t', tok = next t in
    File.Fmt.stdout
    |> Fmt.fmt "  "
    |> Token.pp tok
    |> Fmt.fmt "\n"
    |> ignore;
    match tok with
    | Tok_end_of_input _ -> ()
    | _ -> fn t'
  end in
  File.Fmt.stdout
  |> String.fmt ~alt:true ~pretty:true s
  |> Fmt.fmt "\n"
  |> ignore;
  let t = init (Text.of_string_slice (String.C.Slice.of_string s)) in
  fn t
