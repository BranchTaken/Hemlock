open! Basis.Rudiments
open! Basis
open Array
open Format

let rec fn arr hd cursor tl =
  let index = Cursor.index cursor in
  printf "index=%a" Uns.pp index;
  printf ", container %a arr" Cmp.pp (cmp Uns.cmp (Cursor.container cursor) arr);
  let hd_cursor = Cursor.cmp hd cursor in
  printf ", hd %a cursor" Cmp.pp hd_cursor;
  let cursor_tl = Cursor.cmp cursor tl in
  printf ", cursor %a tl" Cmp.pp cursor_tl;
  let () = match hd_cursor with
    | Lt -> printf ", lget=%a" Uns.pp (Cursor.lget cursor)
    | Eq -> printf ", lget=_"
    | Gt -> not_reached ()
  in
  let () = match cursor_tl with
    | Lt -> printf ", rget=%a" Uns.pp (Cursor.rget cursor)
    | Eq -> printf ", rget=_"
    | Gt -> not_reached ()
  in
  printf "\n";

  let length = length arr in
  assert (Cursor.(=)
      (Cursor.seek (Uns.to_sint index) hd)
      cursor);
  assert (Cursor.(=)
      hd
      (Cursor.seek (Sint.neg (Uns.to_sint index)) cursor)
  );
  assert (Cursor.(=)
      (Cursor.seek (Uns.to_sint (length - index)) cursor)
      tl
  );
  assert (Cursor.(=)
      cursor
      (Cursor.seek (Sint.neg (Uns.to_sint (length - index))) tl)
  );

  match cursor_tl with
  | Lt -> begin
      let cursor' = Cursor.succ cursor in
      assert Cursor.(cursor = (pred cursor'));
      fn arr hd cursor' tl
    end
  | Eq | Gt -> ()

let test () =
  let arrs = [
    [||];
    [|0L|];
    [|0L; 1L|];
    [|0L; 1L; 2L|];
  ] in
  printf "@[<h>";
  List.iter arrs ~f:(fun arr ->
    printf "--- %a ---\n" (pp Uns.pp) arr;
    let hd = Cursor.hd arr in
    fn arr hd hd (Cursor.tl arr)
  );
  printf "@]"

let _ = test ()
