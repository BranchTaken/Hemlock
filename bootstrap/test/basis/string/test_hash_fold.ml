open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  printf "@[<h>";
  let rec test strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        printf "hash_fold %a -> %a\n"
          xpp s Hash.xpp (Hash.t_of_state (hash_fold s Hash.State.empty));
        test strs'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3 implementation. *)
  let strings = [""; "hello"; "hello_goodbye"; "<_>"; "«»"; "‡"; "𐆗"] in
  test strings;
  printf "@]"

let _ = test ()
