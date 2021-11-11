open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test kvs = begin
    let ordmap = of_alist (module Uns) kvs in
    File.Fmt.stdout
    |> Fmt.fmt "of_alist "
    |> (List.pp (pp_kv_pair String.pp)) kvs
    |> Fmt.fmt "; to_alist -> "
    |> (List.pp (pp_kv_pair String.pp)) (to_alist ordmap)
    |> Fmt.fmt "; to_array -> "
    |> (Array.pp (pp_kv_pair String.pp)) (to_array ordmap)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_alists = [
    [];
    [(0L, "0")];
    [(0L, "0"); (1L, "1")];
    [(0L, "0"); (1L, "1"); (2L, "2")];
    [(0L, "0"); (1L, "1"); (66L, "66")];
    [(0L, "0"); (1L, "1"); (66L, "66"); (91L, "91")];
  ] in
  List.iter test_alists ~f:(fun kvs ->
    test kvs
  )

let _ = test ()
