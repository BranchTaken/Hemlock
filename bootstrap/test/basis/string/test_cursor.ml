open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_fwd s = begin
    let rec fn cursor i_prev = begin
      match C.Cursor.(cursor = (tl s)) with
      | true -> File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = C.Cursor.bindex cursor in
          assert B.Cursor.((at i s) = (C.Cursor.to_bcursor cursor));
          let () = if Uns.(i_prev <> max_value) then
              Range.iter (0L =:< (i - i_prev)) ~f:(fun j ->
                assert B.Cursor.((near (i_prev + j) s) = (at i_prev s));
              )
          in
          File.Fmt.stdout
          |> Basis.Fmt.fmt "            "
          |> C.Cursor.pp cursor
          |> Basis.Fmt.fmt "="
          |> fmt (of_codepoint (C.Cursor.rget cursor))
          |> Basis.Fmt.fmt "\n"
          |> ignore;
          let cp, cursor' = C.Cursor.next cursor in
          assert Codepoint.(cp = C.Cursor.rget cursor);
          assert C.Cursor.(pred cursor' = cursor);
          fn (C.Cursor.succ cursor) i
        end
    end in
    File.Fmt.stdout |> Basis.Fmt.fmt "cursor fwd:\n" |> ignore;
    fn (C.Cursor.hd s) Uns.max_value
  end in
  let test_rev s = begin
    let rec fn cursor i_prev = begin
      match C.Cursor.(cursor = (hd s)) with
      | true -> File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
      | false -> begin
          let i = C.Cursor.bindex cursor in
          assert B.Cursor.((at i s) = (C.Cursor.to_bcursor cursor));
          let () = if Uns.(i_prev <> max_value) then
              Range.iter (0L =:< (i_prev - i)) ~f:(fun j ->
                assert B.Cursor.((near (i + j) s) = (at i s));
              )
          in
          File.Fmt.stdout
          |> Basis.Fmt.fmt "            "
          |> C.Cursor.pp cursor
          |> Basis.Fmt.fmt "="
          |> fmt (of_codepoint (C.Cursor.lget cursor))
          |> Basis.Fmt.fmt "\n"
          |> ignore;
          let cp, cursor' = C.Cursor.prev cursor in
          assert Codepoint.(cp = C.Cursor.lget cursor);
          assert C.Cursor.(succ cursor' = cursor);
          fn (C.Cursor.pred cursor) i
        end
    end in
    File.Fmt.stdout |> Basis.Fmt.fmt "cursor rev:\n" |> ignore;
    fn (C.Cursor.tl s) Uns.max_value
  end in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  List.iter strs ~f:(fun s ->
    test_fwd s;
    test_rev s;
  )

let _ = test ()
