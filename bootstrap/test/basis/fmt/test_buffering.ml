open! Basis.Rudiments
open! Basis
open Fmt

let test () =
  let fn bufsize = begin
    File.Fmt.stdout
    |> fmt "bufsize=" |> fmt ~width:2L (Uns.to_string bufsize) |> fmt " -> " |> flush |> ignore;
    File.Fmt.of_t ~bufsize File.stdout
    |> fmt "0" |> fmt "1" |> fmt "2" |> fmt "3" |> fmt "4" |> fmt "5" |> fmt "6" |> fmt "7"
    |> fmt "8" |> fmt "9" |> fmt "\n" |> flush |> ignore
  end in
  Range.Uns.iter (0L =:< 13L) ~f:(fun i -> fn i)

let _ = test ()
