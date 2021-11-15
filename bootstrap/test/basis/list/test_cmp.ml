open! Basis.Rudiments
open! Basis
open List

let test () =
  let test_cmp lst0 lst1 = begin
    File.Fmt.stdout
    |> Fmt.fmt " -> "
    |> Cmp.pp (cmp Uns.cmp lst0 lst1)
    |> Fmt.fmt "\n"
    |> ignore
  end in
  let rec test_with_lists lists lists0 lists1 = begin
    match lists0, lists1 with
    | [], _ -> ()
    | _ :: lists0', [] -> test_with_lists lists lists0' lists
    | list0 :: _, list1 :: lists1' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "cmp "
        |> (pp Uns.pp) list0
        |> Fmt.fmt " "
        |> (pp Uns.pp) list1
        |> Fmt.fmt " -> "
        |> ignore;
        test_cmp list0 list1;
        test_with_lists lists lists0 lists1'
      end
  end in
  let lists = [
    [];
    [0L];
    [1L];
    [0L; 1L];
    [0L; 2L];
    [1L; 2L]
  ] in
  test_with_lists lists lists lists

let _ = test ()
