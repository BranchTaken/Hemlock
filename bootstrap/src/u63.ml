include Rudiments_int

let to_isize t =
  isize_of_usize t

let of_isize x =
  usize_of_isize x

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "limits" =
  let open Format in

  printf "num_bits=%a\n" pp num_bits;
  printf "min_value=%a\n" pp_x min_value;
  printf "max_value=%a\n" pp_x max_value;

  [%expect{|
    num_bits=63
    min_value=0x0000000000000000
    max_value=0x7fffffffffffffff
    |}]

let%expect_test "rel" =
  let open Format in
  let fn x y = begin
    printf "cmp %a %a -> %a\n" pp_x x pp_x y Cmp.pp (cmp x y);
    printf "%a >= %a -> %b\n" pp_x x pp_x y (x >= y);
    printf "%a <= %a -> %b\n" pp_x x pp_x y (x <= y);
    printf "%a = %a -> %b\n" pp_x x pp_x y (x = y);
    printf "%a > %a -> %b\n" pp_x x pp_x y (x > y);
    printf "%a < %a -> %b\n" pp_x x pp_x y (x < y);
    printf "%a <> %a -> %b\n" pp_x x pp_x y (x <> y);
    printf "ascending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (ascending x y);
    printf "descending %a %a -> %a\n" pp_x x pp_x y Cmp.pp (descending x y);
  end in
  fn 0 0x4000_0000_0000_0000;
  printf "\n";
  fn 0 0x7fff_ffff_ffff_ffff;
  printf "\n";
  fn 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff;
  let fn2 t min max = begin
    printf "\n";
    printf "clamp %a ~min:%a ~max:%a -> %a\n"
      pp_x t pp_x min pp_x max pp_x (clamp t ~min ~max);
    printf "between %a ~low:%a ~high:%a -> %b\n"
      pp_x t pp_x min pp_x max (between t ~low:min ~high:max);
  end in
  fn2 0x3fff_ffff_ffff_fffe 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x3fff_ffff_ffff_ffff 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0000 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0001 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;
  fn2 0x4000_0000_0000_0002 0x3fff_ffff_ffff_ffff 0x4000_0000_0000_0001;

  [%expect{|
    cmp 0x0000000000000000 0x4000000000000000 -> Lt
    0x0000000000000000 >= 0x4000000000000000 -> false
    0x0000000000000000 <= 0x4000000000000000 -> true
    0x0000000000000000 = 0x4000000000000000 -> false
    0x0000000000000000 > 0x4000000000000000 -> false
    0x0000000000000000 < 0x4000000000000000 -> true
    0x0000000000000000 <> 0x4000000000000000 -> true
    ascending 0x0000000000000000 0x4000000000000000 -> Lt
    descending 0x0000000000000000 0x4000000000000000 -> Gt

    cmp 0x0000000000000000 0x7fffffffffffffff -> Lt
    0x0000000000000000 >= 0x7fffffffffffffff -> false
    0x0000000000000000 <= 0x7fffffffffffffff -> true
    0x0000000000000000 = 0x7fffffffffffffff -> false
    0x0000000000000000 > 0x7fffffffffffffff -> false
    0x0000000000000000 < 0x7fffffffffffffff -> true
    0x0000000000000000 <> 0x7fffffffffffffff -> true
    ascending 0x0000000000000000 0x7fffffffffffffff -> Lt
    descending 0x0000000000000000 0x7fffffffffffffff -> Gt

    cmp 0x4000000000000000 0x3fffffffffffffff -> Gt
    0x4000000000000000 >= 0x3fffffffffffffff -> true
    0x4000000000000000 <= 0x3fffffffffffffff -> false
    0x4000000000000000 = 0x3fffffffffffffff -> false
    0x4000000000000000 > 0x3fffffffffffffff -> true
    0x4000000000000000 < 0x3fffffffffffffff -> false
    0x4000000000000000 <> 0x3fffffffffffffff -> true
    ascending 0x4000000000000000 0x3fffffffffffffff -> Gt
    descending 0x4000000000000000 0x3fffffffffffffff -> Lt

    clamp 0x3ffffffffffffffe ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x3fffffffffffffff
    between 0x3ffffffffffffffe ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> false

    clamp 0x3fffffffffffffff ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x3fffffffffffffff
    between 0x3fffffffffffffff ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000000 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000000
    between 0x4000000000000000 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000001 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000001
    between 0x4000000000000001 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> true

    clamp 0x4000000000000002 ~min:0x3fffffffffffffff ~max:0x4000000000000001 -> 0x4000000000000001
    between 0x4000000000000002 ~low:0x3fffffffffffffff ~high:0x4000000000000001 -> false
    |}]

let%expect_test "narrowing" =
  let open Format in
  printf "max_value + 1 -> %a\n" pp_x (max_value + 1);
  printf "min_value - 1 -> %a\n" pp_x (min_value - 1);
  printf "max_value * 15 -> %a\n" pp_x (max_value * 15);

  [%expect{|
    max_value + 1 -> 0x0000000000000000
    min_value - 1 -> 0x7fffffffffffffff
    max_value * 15 -> 0x7ffffffffffffff1
    |}]
