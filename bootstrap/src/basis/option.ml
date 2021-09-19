open Rudiments0

module T = struct
  type 'a t = 'a option =
    | None
    | Some of 'a
  type 'a elm = 'a

  let hash_fold hash_fold_a t state =
    match t with
    | None -> state |> Uns.hash_fold 0
    | Some a -> state |> Uns.hash_fold 1 |> hash_fold_a a

  let cmp cmp_a t0 t1 =
    let open Cmp in
    match t0, t1 with
    | None, None -> Eq
    | None, Some _ -> Lt
    | Some _, None -> Gt
    | Some a0, Some a1 -> cmp_a a0 a1

  module Cursor = struct
    module T = struct
      type 'a container = 'a t
      type 'a t = {
        option: 'a container;
        index: uns;
      }

      let cmp t0 t1 =
        (* == is excessively vague in OCaml. *)
        assert ((t0.option == t1.option) || (Stdlib.( = ) t0.option t1.option));
        Uns.cmp t0.index t1.index

      let hd option =
        {option; index=0}

      let tl option =
        match option with
        | None -> {option; index=0}
        | Some _ -> {option; index=1}

      let pred t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0 -> halt "At beginning of option"
        | Some _, _ -> begin
            assert (t.index = 1);
            {t with index=0}
          end

      let succ t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1 -> halt "At end of option"
        | Some _, _ -> begin
            assert (t.index = 0);
            {t with index=1}
          end

      let lget t =
        match t.option, t.index with
        | None, _ -> halt "At beginning of option"
        | Some _, 0 -> halt "At beginning of option"
        | Some v, _ -> begin
            assert (t.index = 1);
            v
          end

      let rget t =
        match t.option, t.index with
        | None, _ -> halt "At end of option"
        | Some _, 1 -> halt "At end of option"
        | Some v, _ -> begin
            assert (t.index = 0);
            v
          end

      let prev t =
        lget t, pred t

      let next t =
        rget t, succ t

      let container t =
        t.option

      let index t =
        t.index

      let seek i t =
        match int_of_sint i with
        | -1 -> pred t
        | 0 -> t
        | 1 -> succ t
        | _ -> halt "Out of bounds"
    end
    include T
    include Cmpable.MakePoly(T)
  end

  let length = function
    | None -> 0
    | Some _ -> 1

  let is_empty t =
    (length t) = 0
end
include T
include ContainerCommon.MakePolyFold(T)
include ContainerArray.MakePolyArray(T)

let pp pp_a ppf = function
  | Some a -> Format.fprintf ppf "@[<h>Some@ %a@]" pp_a a
  | None -> Format.fprintf ppf "None"

let is_some = function
  | Some _ -> true
  | None -> false

let is_none = function
  | Some _ -> false
  | None -> true

let value ~default t =
  match t with
  | Some a -> a
  | None -> default

let value_hlt = function
  | Some a -> a
  | None -> halt "No value"

let some_if b a =
  match b with
  | true -> Some a
  | false -> None

let both ta tb =
  match ta, tb with
  | Some a, Some b -> Some (a, b)
  | Some _, None
  | None, Some _
  | None, None -> None

let first_some t0 t1 =
  match t0, t1 with
  | Some a, _
  | None, Some a -> Some a
  | None, None -> None

let filter ~f t =
  match t with
  | Some a -> begin
      match f a with
      | false -> None
      | true -> Some a
    end
  | None -> None

let value_map ~default ~f t =
  match t with
  | Some a -> f a
  | None -> default

let merge ~f t0 t1 =
  match t0, t1 with
  | Some a0, Some a1 -> Some (f a0 a1)
  | Some a, None
  | None, Some a -> Some a
  | None, None -> None

let map2 ~f ta tb =
  match ta, tb with
  | Some a, Some b -> Some (f a b)
  | _, _ -> None

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | option :: options' -> begin
        printf "hash_fold %a -> %a\n"
          (pp Uns.pp) option
          Hash.pp (Hash.t_of_state
            (hash_fold Uns.hash_fold option Hash.State.empty));
        fn options'
      end
  in
  let options = [
    None;
    (Some 0);
    (Some 1)
  ] in
  fn options;
  printf "@]";

  [%expect{|
    hash_fold None -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold Some 0 -> 0x03ed_688f_024a_5170_df17_bbe2_160e_8fa0u128
    hash_fold Some 1 -> 0x2c31_1233_8879_fc2a_f407_45af_d2ef_7351u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold Unit.hash_fold None
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2));

  [%expect{|
    |}]

let%expect_test "pp" =
  let open Format in
  printf "@[<h>";
  printf "Some 42 -> %a\n" (pp Uns.pp) (Some 42);
  printf "None -> %a\n" (pp Uns.pp) None;
  printf "@]";

  [%expect{|
    Some 42 -> Some 42
    None -> None
  |}]

let%expect_test "is_some,is_none" =
  let open Format in
  printf "@[<h>";
  List.iter [Some 42; None] ~f:(fun o ->
    printf "is_some %a -> %b\n" (pp Uns.pp) o (is_some o);
    printf "is_none %a -> %b\n" (pp Uns.pp) o (is_none o);
  );
  printf "@]";

  [%expect{|
    is_some Some 42 -> true
    is_none Some 42 -> false
    is_some None -> false
    is_none None -> true
  |}]

let%expect_test "value" =
  let open Format in
  printf "@[<h>";
  List.iter [Some 42; None] ~f:(fun o ->
    printf "value %a -> %a\n"
      (pp Uns.pp) o Uns.pp (value ~default:13 o)
  );
  printf "@]";

  [%expect{|
    value Some 42 -> 42
    value None -> 13
  |}]

let%expect_test "some_if" =
  let open Format in
  printf "@[<h>";
  List.iter [false; true] ~f:(fun b ->
    let a = 42 in
    printf "some_if %b %a -> %a\n"
      b
      Uns.pp a
      (pp Uns.pp) (some_if b a)
  );
  printf "@]";

  [%expect{|
    some_if false 42 -> None
    some_if true 42 -> Some 42
  |}]

let%expect_test "both" =
  let open Format in
  let pp_ab ppf (a, b) = fprintf ppf "(%a, %a)" Uns.pp a String.pp b in
  printf "@[<h>";
  List.iter [Some 42; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "both (%a) (%a) -> %a\n"
        (pp Uns.pp) o0
        (pp String.pp) o1
        (pp pp_ab) (both o0 o1)
    )
  );
  printf "@]";

  [%expect{|
    both (Some 42) (Some "hi") -> Some (42, "hi")
    both (Some 42) (None) -> None
    both (None) (Some "hi") -> None
    both (None) (None) -> None
  |}]

let%expect_test "first_some" =
  let open Format in
  printf "@[<h>";
  List.iter [Some 42; None] ~f:(fun o0 ->
    List.iter [Some 13; None] ~f:(fun o1 ->
      printf "first_some (%a) (%a) -> %a\n"
        (pp Uns.pp) o0
        (pp Uns.pp) o1
        (pp Uns.pp) (first_some o0 o1)
    )
  );
  printf "@]";

  [%expect{|
    first_some (Some 42) (Some 13) -> Some 42
    first_some (Some 42) (None) -> Some 42
    first_some (None) (Some 13) -> Some 13
    first_some (None) (None) -> None
  |}]

let%expect_test "filter" =
  let open Format in
  printf "@[<h>";
  List.iter [false; true] ~f:(fun b ->
    List.iter [Some 42; None] ~f:(fun o ->
      printf "filter %a ~f:(fun _ -> %b) -> %a\n"
        (pp Uns.pp) o
        b
        (pp Uns.pp) (filter o ~f:(fun _ -> b))
    )
  );
  printf "@]";

  [%expect{|
    filter Some 42 ~f:(fun _ -> false) -> None
    filter None ~f:(fun _ -> false) -> None
    filter Some 42 ~f:(fun _ -> true) -> Some 42
    filter None ~f:(fun _ -> true) -> None
  |}]

let%expect_test "value_map" =
  let open Format in
  printf "@[<h>";
  let default = 13 in
  let replacement = 43 in
  List.iter [Some 42; None] ~f:(fun o ->
    printf "value_map %a ~default:%a ~f:(fun _ -> %a) -> %a\n"
      (pp Uns.pp) o
      Uns.pp default
      Uns.pp replacement
      Uns.pp (value_map o ~default ~f:(fun _ -> replacement))
  );
  printf "@]";

  [%expect{|
    value_map Some 42 ~default:13 ~f:(fun _ -> 43) -> 43
    value_map None ~default:13 ~f:(fun _ -> 43) -> 13
  |}]

let%expect_test "merge" =
  let open Format in
  printf "@[<h>";
  let replacement = 77 in
  List.iter [Some 42; None] ~f:(fun o0 ->
    List.iter [Some 43; None] ~f:(fun o1 ->
      printf "merge (%a) (%a) ~f:(fun _ _ -> %a) -> %a\n"
        (pp Uns.pp) o0
        (pp Uns.pp) o1
        Uns.pp replacement
        (pp Uns.pp) (merge o0 o1 ~f:(fun _ _ -> replacement))
    )
  );
  printf "@]";

  [%expect{|
    merge (Some 42) (Some 43) ~f:(fun _ _ -> 77) -> Some 77
    merge (Some 42) (None) ~f:(fun _ _ -> 77) -> Some 42
    merge (None) (Some 43) ~f:(fun _ _ -> 77) -> Some 43
    merge (None) (None) ~f:(fun _ _ -> 77) -> None
  |}]

let%expect_test "map2" =
  let open Format in
  let pp_ab ppf (a, b) = fprintf ppf "(%a, %a)" Uns.pp a String.pp b in
  printf "@[<h>";
  List.iter [Some 42; None] ~f:(fun o0 ->
    List.iter [Some "hi"; None] ~f:(fun o1 ->
      printf "map2 (%a) (%a) ~f:(fun a b -> (a, b)) -> %a\n"
        (pp Uns.pp) o0
        (pp String.pp) o1
        (pp pp_ab) (map2 o0 o1 ~f:(fun a b -> (a, b)))
    )
  );
  printf "@]";

  [%expect{|
    map2 (Some 42) (Some "hi") ~f:(fun a b -> (a, b)) -> Some (42, "hi")
    map2 (Some 42) (None) ~f:(fun a b -> (a, b)) -> None
    map2 (None) (Some "hi") ~f:(fun a b -> (a, b)) -> None
    map2 (None) (None) ~f:(fun a b -> (a, b)) -> None
  |}]
