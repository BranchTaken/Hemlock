open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset

let test () =
  let test_fwd ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordset)) with
      | true -> File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordset)) = cursor);
          File.Fmt.stdout
          |> Fmt.fmt "            "
          |> cursor_pp cursor
          |> Fmt.fmt "="
          |> Uns.pp (Cursor.rget cursor)
          |> Fmt.fmt "\n"
          |> ignore;
          fn (Cursor.succ cursor)
        end
    end in
    File.Fmt.stdout
    |> Fmt.fmt "cursor fwd:\n"
    |> ignore;
    fn (Cursor.hd ordset);
  end in
  let test_rev ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordset)) with
      | true -> File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordset)) = cursor);
          File.Fmt.stdout
          |> Fmt.fmt "            "
          |> cursor_pp cursor
          |> Fmt.fmt "="
          |> Uns.pp (Cursor.lget cursor)
          |> Fmt.fmt "\n"
          |> ignore;
          fn (Cursor.pred cursor)
        end
    end in
    File.Fmt.stdout
    |> Fmt.fmt "cursor rev:\n"
    |> ignore;
    fn (Cursor.tl ordset);
  end in
  let test ms = begin
    let ordset = of_array (module Uns) ms in
    File.Fmt.stdout
    |> Fmt.fmt "of_array "
    |> (Array.pp Uns.pp) ms
    |> Fmt.fmt " -> "
    |> fmt ordset
    |> Fmt.fmt "\n"
    |> ignore;
    test_fwd ordset;
    test_rev ordset
  end in
  let test_arrays = [
    [||];
    [|0L; 1L; 4L; 5L; 3L; 2L|];
  ] in
  List.iter test_arrays ~f:(fun ms ->
    test ms
  )

let _ = test ()
