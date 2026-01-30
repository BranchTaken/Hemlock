open! Basis.Rudiments
open! Basis
open Bitset

let test () =
  (* test is n^2 time complexity, so keep elms small. *)
  let rec fn elms i e bitset = begin
    let n = Array.length elms in
    match i < n with
    | false -> bitset
    | true -> begin
        let elm = Array.get i elms in
        let bitset' = remove elm (fn elms (succ i) e (insert elm bitset)) in
        assert (equal bitset bitset');
        assert (equal bitset (union bitset bitset'));
        assert (equal bitset (inter bitset bitset'));
        assert (equal e (diff bitset bitset'));
        bitset'
      end
  end in
  (* Random order exercises nonmonotonic insertion order. *)
  let elms = [|114L; 2431L; 143L; 3502L; 1570L; 2155L; 3847L; 2547L; 3883L; 665L; 1250L; 3817L;
    2095L; 3937L; 1425L; 3057L; 98L; 2442L; 2015L; 3010L; 2010L; 2072L; 1149L; 3884L; 1051L; 3896L;
    171L; 3781L; 2513L; 2699L; 241L; 1416L; 1143L; 142L; 135L; 2776L; 212L; 2971L; 2069L; 2531L;
    1640L; 2669L; 1772L; 1203L; 1039L; 3058L; 187L; 670L; 1608L; 1470L; 3268L; 994L; 1861L; 748L;
    1196L; 2074L; 21L; 3349L; 1843L; 1323L; 2413L; 3956L; 1534L; 3997L; 1890L; 317L; 530L; 3008L;
    3556L; 1058L; 854L; 2195L; 4044L; 1115L; 3326L; 1763L; 2426L; 3168L; 4029L; 2387L; 2689L; 2460L;
    1776L; 1721L; 683L; 393L; 3768L; 2033L; 2085L; 3806L; 3531L; 37L; 7L; 607L; 2984L; 1933L; 1599L;
    112L; 1943L; 382L|] in
  let e = empty in
  let _ = fn elms 0L e e in
  ()

let _ = test ()
