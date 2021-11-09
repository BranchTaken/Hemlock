open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fwd s = begin
    let rec fn cursor i_prev = begin
      match C.Cursor.(cursor = (tl s)) with
      | true -> printf "\n"
      | false -> begin
          let i = C.Cursor.bindex cursor in
          assert B.Cursor.((at i s) = (C.Cursor.to_bcursor cursor));
          let () = if Uns.(i_prev <> max_value) then
              Range.iter (0L =:< (i - i_prev)) ~f:(fun j ->
                assert B.Cursor.((near (i_prev + j) s) = (at i_prev s));
              )
          in
          printf "            %a=%s\n"
            C.Cursor.xpp cursor (of_codepoint (C.Cursor.rget cursor));
          let cp, cursor' = C.Cursor.next cursor in
          assert Codepoint.(cp = C.Cursor.rget cursor);
          assert C.Cursor.(pred cursor' = cursor);
          fn (C.Cursor.succ cursor) i
        end
    end in
    printf "cursor fwd:\n";
    fn (C.Cursor.hd s) Uns.max_value
  end in
  let test_rev s = begin
    let rec fn cursor i_prev = begin
      match C.Cursor.(cursor = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = C.Cursor.bindex cursor in
          assert B.Cursor.((at i s) = (C.Cursor.to_bcursor cursor));
          let () = if Uns.(i_prev <> max_value) then
              Range.iter (0L =:< (i_prev - i)) ~f:(fun j ->
                assert B.Cursor.((near (i + j) s) = (at i s));
              )
          in
          printf "            %a=%s\n"
            C.Cursor.xpp cursor (of_codepoint (C.Cursor.lget cursor));
          let cp, cursor' = C.Cursor.prev cursor in
          assert Codepoint.(cp = C.Cursor.lget cursor);
          assert C.Cursor.(succ cursor' = cursor);
          fn (C.Cursor.pred cursor) i
        end
    end in
    printf "cursor rev:\n";
    fn (C.Cursor.tl s) Uns.max_value
  end in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    test_fwd s;
    test_rev s;
  );
  printf "@]"

let _ = test ()
