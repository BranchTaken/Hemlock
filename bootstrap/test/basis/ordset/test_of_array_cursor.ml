open! Basis.Rudiments
open! Basis
open OrdsetTest
open Ordset
open Format

let test () =
  printf "@[";
  let test_fwd ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (tl ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            cursor_pp cursor
            Uns.pp (Cursor.rget cursor);
          fn (Cursor.succ cursor)
        end
    end in
    printf "cursor fwd:@\n";
    fn (Cursor.hd ordset);
  end in
  let test_rev ordset = begin
    let rec fn cursor = begin
      match Cursor.(cursor = (hd ordset)) with
      | true -> printf "@\n"
      | false -> begin
          let i = Cursor.index cursor in
          assert Cursor.((seek (Uns.bits_to_sint i) (hd ordset)) = cursor);
          printf "            %a=%a@\n"
            cursor_pp cursor
            Uns.pp (Cursor.lget cursor);
          fn (Cursor.pred cursor)
        end
    end in
    printf "cursor rev:@\n";
    fn (Cursor.tl ordset);
  end in
  let test ms = begin
    let ordset = of_array (module Uns) ms in
    printf "of_array %a ->@,%a@\n"
      (Array.pp Uns.pp) ms
      pp ordset;
    test_fwd ordset;
    test_rev ordset
  end in
  let test_arrays = [
    [||];
    [|0L; 1L; 4L; 5L; 3L; 2L|];
  ] in
  List.iter test_arrays ~f:(fun ms ->
    test ms
  );
  printf "@]"

let _ = test ()
