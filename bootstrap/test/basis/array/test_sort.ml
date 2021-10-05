open! Basis.Rudiments
open! Basis
open! ArrayTest
open Array

type sort_elm = {
  key: uns; (* Random, possibly non-unique. *)
  sn: uns; (* Sequential in initial array. *)
}

let test () =
  let gen_array len = begin
    let key_limit =
      if len > 0L then len
      else 1L
    in
    init len ~f:(fun i ->
      {key=Uns.extend_of_int (Stdlib.Random.int (Int64.to_int key_limit)); sn=i})
  end in
  let cmp elm0 elm1 =
    Uns.cmp elm0.key elm1.key
  in
  let test_sort arr = begin
    let open Cmp in
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
  iter_oc 0L 258L (fun len ->
    iter_oc 1L 11L (fun _ ->
      test_sort (gen_array len)
    )
  )

let _ = test ()
