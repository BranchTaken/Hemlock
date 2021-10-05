open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap
open Format

let test () =
  printf "@[";
  let test_fwd ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordmap)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordmap)) = cursor);
          printf "            %a=%a@\n"
            cursor_pp cursor
            (pp_kv String.pp) (Cursor.rget cursor);
          fn (Cursor.succ cursor)
        end
    end in
    printf "cursor fwd:@\n";
    fn (Cursor.hd ordmap);
  end in
  let test_rev ordmap = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordmap)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordmap)) = cursor);
          printf "            %a=%a@\n"
            cursor_pp cursor
            (pp_kv String.pp) (Cursor.lget cursor);
          fn (Cursor.pred cursor)
        end
    end in
    printf "cursor rev:@\n";
    fn (Cursor.tl ordmap);
  end in
  let test kvs = begin
    let ordmap = of_array (module Uns) kvs in
    printf "of_array %a ->@,%a@\n"
      (Array.pp (pp_kv String.pp)) kvs
      (pp String.pp) ordmap;
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
  );
  printf "@]"

let _ = test ()
