open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test kvs = begin
    let map = of_alist (module UnsTestCmper) kvs in
    File.Fmt.stdout
    |> Fmt.fmt "of_alist "
    |> (List.pp (pp_kv String.pp)) kvs
    |> Fmt.fmt "; to_alist -> "
    |> (List.pp (pp_kv String.pp)) (to_alist map)
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
