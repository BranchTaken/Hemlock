open Rudiments

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b

let pp pp_a pp_b ppf = function
  | Ok a -> Format.fprintf ppf "@[<h>Ok@ %a@]" pp_a a
  | Error b -> Format.fprintf ppf "@[<h>Error@ %a@]" pp_b b

let ok_if b ~error =
  match b with
  | false -> Error error
  | true -> Ok ()

let error_if b ~ok =
  match b with
  | false -> Ok ok
  | true -> Error ()

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok_opt = function
  | Ok a -> Some a
  | Error _ -> None

let ok_hlt = function
  | Ok a -> a
  | Error _ -> halt "Not Ok"

let error_opt = function
  | Ok _ -> None
  | Error b -> Some b

let error_hlt = function
  | Ok _ -> halt "Not Error"
  | Error b -> b

let oks_errors ts =
  let rec fn ts = begin
    match ts with
    | [] -> [], []
    | Ok ok :: ts' -> begin
        let oks, errors = fn ts' in
        (ok :: oks), errors
      end
    | Error error :: ts' -> begin
        let oks, errors = fn ts' in
        oks, (error :: errors)
      end
  end in
  fn ts

let all ts =
  let oks, errors = oks_errors ts in
  match oks, errors with
  | _, _ :: _ -> Error errors
  | _, _ -> Ok oks

let all_hlt ts =
  match all ts with
  | Ok oks -> oks
  | Error _ -> halt "Not all Ok"

let ok_ignore ts =
  match all ts with
  | Ok _ -> Ok ()
  | Error errors -> Error errors

let ok_ignore_hlt ts =
  let _ = all_hlt ts in
  ()

let error_ignore ts =
  match all ts with
  | Ok oks -> Ok oks
  | Error _ -> Error ()

let error_ignore_hlt ts =
  match error_ignore ts with
  | Ok _ -> halt "Not all Error"
  | Error _ -> ()

let map_ok t ~f =
  match t with
  | Ok ok -> Ok (f ok)
  | Error error -> Error error

let map_error t ~f =
  match t with
  | Ok ok -> Ok ok
  | Error error -> Error (f error)

let merge t0 t1 ~ok ~error =
  match t0, t1 with
  | Ok ok0, Ok ok1 -> Ok (ok ok0 ok1)
  | Error error, Ok _
  | Ok _, Error error -> Error error
  | Error error0, Error error1 -> Error (error error0 error1)

(******************************************************************************)
(* Begin tests. *)

let%expect_test "pp" =
  let open Format in
  printf "@[<h>";
  printf "Ok 42 -> %a\n" (pp Usize.pp String.pp) (Ok 42);
  printf "Error \"bang\" -> %a\n" (pp Usize.pp String.pp) (Error "bang");
  printf "@]";

  [%expect{|
    Ok 42 -> Ok 42
    Error "bang" -> Error "bang"
  |}]

let%expect_test "ok_if,error_if" =
  let open Format in
  printf "@[<h>";
  List.iter [true; false] ~f:(fun b ->
    printf "ok_if %b -> %a\n" b (pp Unit.pp String.pp) (ok_if b ~error:"oops");
    printf "error_if %b -> %a\n"
      b (pp String.pp Unit.pp) (error_if b ~ok:"whew")
  );
  printf "@]";

  [%expect{|
    ok_if true -> Ok ()
    error_if true -> Error ()
    ok_if false -> Error "oops"
    error_if false -> Ok "whew"
  |}]

let%expect_test "is_ok,is_error" =
  let open Format in
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "is_ok %a -> %b\n" (pp String.pp String.pp) result (is_ok result);
    printf "is_error %a -> %b\n"
      (pp String.pp String.pp) result (is_error result);
  );
  printf "@]";

  [%expect{|
    is_ok Ok "ok" -> true
    is_error Ok "ok" -> false
    is_ok Error "error" -> false
    is_error Error "error" -> true
  |}]

let%expect_test "ok_opt,error_opt" =
  let open Format in
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "ok_opt %a -> %a\n"
      (pp String.pp String.pp) result
      (Option.pp String.pp) (ok_opt result);
    printf "error_opt %a -> %a\n"
      (pp String.pp String.pp) result
      (Option.pp String.pp) (error_opt result);
  );
  printf "@]";

  [%expect{|
    ok_opt Ok "ok" -> Some "ok"
    error_opt Ok "ok" -> None
    ok_opt Error "error" -> None
    error_opt Error "error" -> Some "error"
  |}]

let%expect_test "all" =
  let open Format in
  let results_lists = [
    [ (Ok "ok0"); (Ok "ok1"); (Ok "ok2")];
    [ (Ok "ok0"); (Error "error0"); (Ok "ok1"); (Error "error1"); (Ok "ok2")];
  ] in
  printf "@[<h>";
  List.iter results_lists ~f:(fun results ->
    printf "all %a -> %a\n"
      (List.pp (pp String.pp String.pp)) results
      (pp (List.pp String.pp) (List.pp String.pp)) (all results)
  );
  printf "@]";

  [%expect{|
    all [Ok "ok0"; Ok "ok1"; Ok "ok2"] -> Ok ["ok0"; "ok1"; "ok2"]
    all [Ok "ok0"; Error "error0"; Ok "ok1"; Error "error1"; Ok "ok2"] -> Error ["error0"; "error1"]
  |}]

let%expect_test "ok_ignore,error_ignore" =
  let open Format in
  let results_lists = [
    [ (Ok "ok0"); (Ok "ok1"); (Ok "ok2")];
    [ (Ok "ok0"); (Error "error0"); (Ok "ok1"); (Error "error1"); (Ok "ok2")];
  ] in
  printf "@[<h>";
  List.iter results_lists ~f:(fun results ->
    printf "ok_ignore %a -> %a\n"
      (List.pp (pp String.pp String.pp)) results
      (pp Unit.pp (List.pp String.pp)) (ok_ignore results);
    printf "error_ignore %a -> %a\n"
      (List.pp (pp String.pp String.pp)) results
      (pp (List.pp String.pp) Unit.pp) (error_ignore results)
  );
  printf "@]";

  [%expect{|
    ok_ignore [Ok "ok0"; Ok "ok1"; Ok "ok2"] -> Ok ()
    error_ignore [Ok "ok0"; Ok "ok1"; Ok "ok2"] -> Ok ["ok0"; "ok1"; "ok2"]
    ok_ignore [Ok "ok0"; Error "error0"; Ok "ok1"; Error "error1"; Ok "ok2"] -> Error ["error0"; "error1"]
    error_ignore [Ok "ok0"; Error "error0"; Ok "ok1"; Error "error1"; Ok "ok2"] -> Error ()
  |}]

let%expect_test "map_ok,map_error" =
  let open Format in
  let f msg = asprintf "%s'" msg in
  printf "@[<h>";
  List.iter [Ok "ok"; Error "error"] ~f:(fun result ->
    printf "map_ok %a -> %a\n"
      (pp String.pp String.pp) result
      (pp String.pp String.pp) (map_ok result ~f);
    printf "map_error %a -> %a\n"
      (pp String.pp String.pp) result
      (pp String.pp String.pp) (map_error result ~f);
  );
  printf "@]";

  [%expect{|
    map_ok Ok "ok" -> Ok "ok'"
    map_error Ok "ok" -> Ok "ok"
    map_ok Error "error" -> Error "error"
    map_error Error "error" -> Error "error'"
  |}]

let%expect_test "merge" =
  let open Format in
  let result_pairs = [
    (Ok "ok0", Ok "ok1");
    (Error "error0", Ok "ok1");
    (Ok "ok0", Error "error1");
    (Error "error0", Error "error1")
  ] in
  printf "@[<h>";
  List.iter result_pairs ~f:(fun (a, b) ->
    let f a b = asprintf "%s + %s" a b in
    printf "merge (%a) (%a) -> (%a)\n"
      (pp String.pp String.pp) a
      (pp String.pp String.pp) b
      (pp String.pp String.pp) (merge a b ~ok:f ~error:f)
  );
  printf "@]";

  [%expect{|
    merge (Ok "ok0") (Ok "ok1") -> (Ok "ok0 + ok1")
    merge (Error "error0") (Ok "ok1") -> (Error "error0")
    merge (Ok "ok0") (Error "error1") -> (Error "error1")
    merge (Error "error0") (Error "error1") -> (Error "error0 + error1")
  |}]
