open! Basis.Rudiments
open! Basis
open Array

let verbose = false

type sort_elm = {
  key: uns; (* Random, possibly non-unique. *)
  sn: uns; (* Sequential in initial array. *)
}

let elm_pp elm formatter =
  formatter
  |> Fmt.fmt "{key="
  |> Uns.pp elm.key
  |> Fmt.fmt ", sn="
  |> Uns.pp elm.sn
  |> Fmt.fmt "}"

let test () =
  let gen_array len = begin
    let key_limit = max 1L len in
    init (0L =:< len) ~f:(fun i ->
      {key=Uns.extend_of_int (Stdlib.Random.int (Int64.to_int key_limit)); sn=i})
  end in
  let cmp elm0 elm1 =
    Uns.cmp elm0.key elm1.key
  in
  let test_sort arr = begin
    let open Cmp in
    let () = match verbose with
      | false -> ()
      | true ->
        File.Fmt.stdout
        |> (fmt ~alt:true elm_pp) arr
        |> Fmt.fmt "\n"
        |> ignore;
    in
    let arr' = sort arr ~cmp in
    assert (is_sorted arr' ~cmp);

    let arr' = sort ~stable:true arr ~cmp in
    assert (is_sorted ~strict:true arr' ~cmp:(fun elm0 elm1 ->
      match cmp elm0 elm1 with
      | Lt -> Cmp.Lt
      | Eq -> Uns.cmp elm0.sn elm1.sn
      | Gt -> Cmp.Gt
    ));
  end in
  Stdlib.Random.init 0;
  Range.iter (0L =:< 258L) ~f:(fun len ->
    Range.iter (0L =:< 10L) ~f:(fun _ ->
      test_sort (gen_array len)
    )
  )

let _ = test ()
