open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fwd s = begin
    let rec fn cursor i_prev = begin
      match Cursor.(cursor = (tl s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursor.bindex cursor in
          assert Cursor.((at s ~bindex:i) = cursor);
          let () = if Uns.(i_prev <> max_value) then
              for j = 0 to i - i_prev - 1 do
                assert Cursor.((near s ~bindex:(i_prev + j))
                  = (at s ~bindex:i_prev));
              done
          in
          printf "            %a=%s\n"
            Cursor.pp cursor (of_codepoint (Cursor.rget cursor));
          let cp, cursor' = Cursor.next cursor in
          assert Codepoint.(cp = Cursor.rget cursor);
          assert Cursor.(pred cursor' = cursor);
          fn (Cursor.succ cursor) i
        end
    end in
    printf "cursor fwd:\n";
    fn (Cursor.hd s);
  end in
  let test_rev s = begin
    let rec fn cursor i_prev = begin
      match Cursor.(cursor = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursor.bindex cursor in
          assert Cursor.((at s ~bindex:i) = cursor);
          let () = if Uns.(i_prev <> max_value) then
              for j = 0 to i_prev - i - 1 do
                assert Cursor.((near s ~bindex:(i + j)) = (at s ~bindex:i));
              done
          in
          printf "            %a=%s\n"
            Cursor.pp cursor (of_codepoint (Cursor.lget cursor));
          let cp, cursor' = Cursor.prev cursor in
          assert Codepoint.(cp = Cursor.lget cursor);
          assert Cursor.(succ cursor' = cursor);
          fn (Cursor.pred cursor) i
        end
    end in
    printf "cursor rev:\n";
    fn (Cursor.tl s);
  end in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    test_fwd s Uns.max_value;
    test_rev s Uns.max_value;
  );
  printf "@]"

let _ = test ()
