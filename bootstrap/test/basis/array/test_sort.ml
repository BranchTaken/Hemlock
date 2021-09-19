open! Basis.Rudiments
open! Basis
open Array

type sort_elm = {
  key: uns; (* Random, possibly non-unique. *)
  sn: uns; (* Sequential in initial array. *)
}

let test () =
  let gen_array len = begin
    let key_limit =
      if len > 0 then len
      else 1
    in
    init len ~f:(fun i -> {key=Stdlib.Random.int key_limit; sn=i})
  end in
  let cmp elm0 elm1 =
    Uns.cmp elm0.key elm1.key
  in
  let test_sort arr = begin
    let arr' = sort arr ~cmp in
    assert (is_sorted arr' ~cmp);

    let arr' = sort ~stable:true arr ~cmp in
    assert (is_sorted ~strict:true arr' ~cmp:(fun elm0 elm1 ->
      match cmp elm0 elm1 with
      | Cmp.Lt -> Cmp.Lt
      | Cmp.Eq -> Uns.cmp elm0.sn elm1.sn
      | Cmp.Gt -> Cmp.Gt
    ));
  end in
  Stdlib.Random.init 0;
  for len = 0 to 257 do
    for _ = 1 to 10 do
      test_sort (gen_array len)
    done
  done

let _ = test ()
