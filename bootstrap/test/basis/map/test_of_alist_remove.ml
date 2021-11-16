open! Basis.Rudiments
open! Basis
open MapTest
open Map

let test () =
  let test k map descr = begin
    validate map;
    File.Fmt.stdout
    |> Fmt.fmt "--- "
    |> Fmt.fmt descr
    |> Fmt.fmt " ---\n"
    |> ignore;
    let map' = remove k map in
    validate map';
    File.Fmt.stdout
    |> Fmt.fmt "remove "
    |> Uns.pp k
    |> Fmt.fmt "\n"
    |> (fmt ~alt:true String.pp) map
    |> Fmt.fmt " ->\n"
    |> (fmt ~alt:true String.pp) map'
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let test_tuples = [
    ([(0L, "0"); (1L, "1")], 2L,            "Not member.");
    ([(0L, "0")], 0L,                       "Member, length 1 -> 0.");
    ([(0L, "0"); (1L, "1")], 1L,            "Member, length 2 -> 1.");
    ([(0L, "0"); (1L, "1"); (2L, "2")], 2L, "Member, length 3 -> 2.");
  ] in
  List.iter test_tuples ~f:(fun (kvs, k, descr) ->
    let map = of_alist (module UnsTestCmper) kvs in
    test k map descr
  )

let _ = test ()
