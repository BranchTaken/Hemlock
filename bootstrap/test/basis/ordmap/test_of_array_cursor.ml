open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test_fwd ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordmap)) with
      | true -> File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordmap)) = cursor);
          File.Fmt.stdout
          |> Fmt.fmt "            "
          |> cursor_pp cursor
          |> Fmt.fmt "="
          |> (pp_kv_pair String.pp) (Cursor.rget cursor)
          |> Fmt.fmt "\n"
          |> ignore;
          fn (Cursor.succ cursor)
        end
    end in
    File.Fmt.stdout
    |> Fmt.fmt "cursor fwd:\n"
    |> ignore;
    fn (Cursor.hd ordmap);
  end in
  let test_rev ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordmap)) with
      | true -> File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordmap)) = cursor);
          File.Fmt.stdout
          |> Fmt.fmt "            "
          |> cursor_pp cursor
          |> Fmt.fmt "="
          |> (pp_kv_pair String.pp) (Cursor.lget cursor)
          |> Fmt.fmt "\n"
          |> ignore;
          fn (Cursor.pred cursor)
        end
    end in
    File.Fmt.stdout
    |> Fmt.fmt "cursor rev:\n"
    |> ignore;
    fn (Cursor.tl ordmap);
  end in
  let test kvs = begin
    let ordmap = of_array (module Uns) kvs in
    File.Fmt.stdout
    |> Fmt.fmt "of_array "
    |> (Array.pp (pp_kv_pair String.pp)) kvs
    |> Fmt.fmt " -> "
    |> (fmt ~alt:true String.pp) ordmap
    |> Fmt.fmt "\n"
    |> ignore;
    validate ordmap;
    test_fwd ordmap;
    test_rev ordmap
  end in
  let test_arrays = [
    [||];
    [|(0L, "0"); (1L, "1"); (4L, "4"); (5L, "5"); (3L, "3"); (2L, "2")|];
  ] in
  List.iter test_arrays ~f:(fun kvs ->
    test kvs
  )

let _ = test ()
