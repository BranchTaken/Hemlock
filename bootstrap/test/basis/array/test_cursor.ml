open! Basis.Rudiments
open! Basis
open Array

let rec fn arr hd cursor tl =
  let index = Cursor.index cursor in
  let _ =
    File.Fmt.stdout
    |> Fmt.fmt "index="
    |> Uns.pp index
    |> Fmt.fmt ", container "
    |> Cmp.pp (cmp Uns.cmp (Cursor.container cursor) arr)
    |> Fmt.fmt " arr"
  in
  let hd_cursor = Cursor.cmp hd cursor in
  let _ =
    File.Fmt.stdout
    |> Fmt.fmt ", hd "
    |> Cmp.pp hd_cursor
    |> Fmt.fmt " cursor"
  in
  let cursor_tl = Cursor.cmp cursor tl in
  let _ =
    File.Fmt.stdout
    |> Fmt.fmt ", cursor "
    |> Cmp.pp cursor_tl
    |> Fmt.fmt " tl"
    |> (fun formatter ->
      match hd_cursor with
      | Lt ->
        formatter
        |> Fmt.fmt ", lget="
        |> Uns.pp (Cursor.lget cursor)
      | Eq ->
        formatter
        |> Fmt.fmt ", lget=_"
      | Gt -> not_reached ()
    )
    |> (fun formatter ->
      match cursor_tl with
      | Lt ->
        formatter
        |> Fmt.fmt ", rget="
        |> Uns.pp (Cursor.rget cursor)
      | Eq ->
        formatter
        |> Fmt.fmt ", rget=_"
      | Gt -> not_reached ()
    )
    |> Fmt.fmt "\n"
  in
  let length = length arr in
  assert (Cursor.(=)
      (Cursor.seek (Uns.bits_to_sint index) hd)
      cursor);
  assert (Cursor.(=)
      hd
      (Cursor.seek (Sint.neg (Uns.bits_to_sint index)) cursor)
  );
  assert (Cursor.(=)
      (Cursor.seek (Uns.bits_to_sint (length - index)) cursor)
      tl
  );
  assert (Cursor.(=)
      cursor
      (Cursor.seek (Sint.neg (Uns.bits_to_sint (length - index))) tl)
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
  List.iter arrs ~f:(fun arr ->
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt "--- "
      |> (pp Uns.pp) arr
      |> Fmt.fmt " ---\n"
    in
    let hd = Cursor.hd arr in
    fn arr hd hd (Cursor.tl arr)
  )

let _ = test ()
