open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test kvs = begin
    let ordmap = of_array (module Uns) kvs in
    File.Fmt.stdout
    |> Fmt.fmt "of_array "
    |> (Array.pp (pp_kv_pair String.pp)) kvs
    |> Fmt.fmt " -> "
    |> (fmt_internals ~alt:true String.pp) ordmap
    |> Fmt.fmt "\n"
    |> ignore;
    validate ordmap
  end in
  let test_arrays = [
    [||];
    [|(0L, "0"); (1L, "1"); (4L, "4"); (5L, "5"); (3L, "3"); (2L, "2")|];
  ] in
  List.iter test_arrays ~f:(fun kvs ->
    test kvs
  )

let _ = test ()
