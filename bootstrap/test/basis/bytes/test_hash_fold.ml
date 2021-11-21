open! Basis.Rudiments
open! Basis
open Bytes

let test () =
  let rec fn strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        let bytes = of_string_slice (String.C.Slice.of_string s) in
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> pp bytes
        |> Fmt.fmt " ("
        |> String.pp s
        |> Fmt.fmt ") -> "
        |> Hash.pp (Hash.t_of_state (hash_fold bytes Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        fn strs'
      end
  end in
  let strs = [""; "hello"; "<"; "«"; "‡"; "𐆗"] in
  fn strs

let _ = test ()
