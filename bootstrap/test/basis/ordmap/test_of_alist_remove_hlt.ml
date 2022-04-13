open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test k ordmap descr = begin
    validate ordmap;
    File.Fmt.stdout
    |> Fmt.fmt "--- "
    |> Fmt.fmt descr
    |> Fmt.fmt " ---\n"
    |> ignore;
    let ordmap' = remove_hlt k ordmap in
    validate ordmap';
    File.Fmt.stdout
    |> Fmt.fmt "remove_hlt "
    |> Uns.pp k
    |> Fmt.fmt "\n    "
    |> (fmt_internals ~alt:true ~width:4L String.pp) ordmap
    |> Fmt.fmt " ->\n    "
    |> (fmt_internals ~alt:true ~width:4L String.pp) ordmap'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_tuples = [
    ([(0L, "0")], 0L,                       "Member, length 1 -> 0.");
    ([(0L, "0"); (1L, "1")], 1L,            "Member, length 2 -> 1.");
    ([(0L, "0"); (1L, "1"); (2L, "2")], 2L, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let ordmap = of_alist (module Uns) kvs in
    test k ordmap descr
  )

let _ = test ()
