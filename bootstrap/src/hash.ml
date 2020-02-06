open Rudiments_int0

type t = usize

let pp ppf t =
  Format.fprintf ppf "0x%016x" t

module State = struct
  type t = usize

  let pp ppf t =
    Format.fprintf ppf "0x%016x" t

  let empty = 0

  let hash_fold a t =
    Hashtbl.seeded_hash t a

  let hash_fold_usize u t =
    hash_fold u t

  let hash_fold_float f t =
    hash_fold f t

  let hash_fold_string s t =
    hash_fold s t

  let seed =
    match Sys.getenv_opt "HEMLOCK_SEED" with
    | None -> begin
        try begin
          let ic = Stdlib.open_in_bin "/dev/random" in
          let buflen = 8 in (* Enough room for a 64-bit integer. *)
          let buf = Stdlib.Bytes.create buflen in
          let _ = Stdlib.really_input ic buf 0 buflen in
          let () = Stdlib.close_in ic in
          Stdlib.Int64.to_int (Stdlib.Bytes.get_int64_ne buf 0)
        end with
        | Stdlib.Sys_error msg -> begin
            Printf.fprintf stderr "Hash.State.seed error: %s\n" msg;
            exit 1
          end
        | Stdlib.End_of_file -> begin
            Printf.fprintf stderr
              "Hash.State.seed error: Reading /dev/random failed\n";
            exit 1
          end
      end
    | Some hemlock_seed -> hash_fold_string hemlock_seed empty
end

let t_of_state state =
  state

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "pp,empty,t_of_state" =
  let open Format in
  printf "@[<h>";
  printf "hash=%a\n" pp (t_of_state State.empty);
  printf "state=%a\n" State.pp State.empty;
  printf "@]";

  [%expect{|
    hash=0x0000000000000000
    state=0x0000000000000000
    |}]

let%expect_test "hash_fold_usize" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold_usize us = begin
    match us with
    | [] -> ()
    | u :: us' -> begin
        printf "hash_fold_usize 0x%016x -> %a\n"
          u pp (t_of_state State.(hash_fold_usize u empty));
        test_hash_fold_usize us'
      end
  end in
  let us = [0; 1; 42; max_int] in
  test_hash_fold_usize us;
  printf "@]";

  [%expect{|
    hash_fold_usize 0x0000000000000000 -> 0x0000000007be548a
    hash_fold_usize 0x0000000000000001 -> 0x0000000034ac84db
    hash_fold_usize 0x000000000000002a -> 0x000000001792870b
    hash_fold_usize 0x3fffffffffffffff -> 0x0000000038c24cfb
    |}]

let%expect_test "hash_fold_float" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold_float floats = begin
    match floats with
    | [] -> ()
    | x :: floats' -> begin
        printf "hash_fold_float %h -> %a\n"
          x pp (t_of_state State.(hash_fold_float x empty));
        test_hash_fold_float floats'
      end
  end in
  let floats = [0.; 1.; 42.; infinity] in
  test_hash_fold_float floats;
  printf "@]";

  [%expect{|
    hash_fold_float 0x0p+0 -> 0x000000000f478b8c
    hash_fold_float 0x1p+0 -> 0x00000000036d56a8
    hash_fold_float 0x1.5p+5 -> 0x00000000346bd9fc
    hash_fold_float infinity -> 0x0000000023ea56fb
    |}]

let%expect_test "hash_fold_string" =
  let open Format in
  printf "@[<h>";
  let rec test_hash_fold_string strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        (* OCaml's %S doesn't format UTF-8 strings correctly, so the outputs
           from this will look a bit nasty until Hemlock has its own printf
           implementation. *)
        printf "hash_fold_string %S -> %a\n"
          s pp (t_of_state State.(hash_fold_string s empty));
        test_hash_fold_string strs'
      end
  end in
  let strings = [""; "<_>"; "«»"; "‡"; "𐆗"] in
  test_hash_fold_string strings;
  printf "@]";

  [%expect{|
    hash_fold_string "" -> 0x0000000000000000
    hash_fold_string "<_>" -> 0x000000003f3b807b
    hash_fold_string "\194\171\194\187" -> 0x000000002779d284
    hash_fold_string "\226\128\161" -> 0x0000000005b67340
    hash_fold_string "\240\144\134\151" -> 0x000000002a276c1f
    |}]
