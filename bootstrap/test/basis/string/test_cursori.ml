open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_fwd s = begin
    let rec fn cursori = begin
      match Cursori.(cursori = (tl s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursori.cindex cursori in
          assert Cursori.((at s ~cindex:i) = cursori);
          printf "             %a=%s\n"
            Cursori.pp cursori (of_codepoint (Cursori.rget cursori));
          fn (Cursori.succ cursori)
        end
    end in
    printf "cursori fwd:\n";
    fn (Cursori.hd s);
  end in

  let test_rev s = begin
    let rec fn cursori = begin
      match Cursori.(cursori = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursori.cindex cursori in
          assert Cursori.((at s ~cindex:i) = cursori);
          printf "             %a=%s\n"
            Cursori.pp cursori (of_codepoint (Cursori.lget cursori));
          fn (Cursori.pred cursori)
        end
    end in
    printf "cursori rev:\n";
    fn (Cursori.tl s);
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
