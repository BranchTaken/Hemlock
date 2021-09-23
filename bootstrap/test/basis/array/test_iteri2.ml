open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let print_uns_arrays arr0 arr1 = begin
    printf "[|";
    iteri2 arr0 arr1 ~f:(fun i elm0 elm1 ->
      if i > 0L then printf "; ";
      printf "(%a, %a)" Uns.pp elm0 Uns.pp elm1
    );
    printf "|]"
  end in
  let test_iter2 arr0 arr1 = begin
    print_uns_arrays arr0 arr1;
    printf "\n"
  end in
  test_iter2 [||] [||];
  test_iter2 [|0L|] [|1L|];
  test_iter2 [|0L; 1L|] [|2L; 3L|];
  test_iter2 [|0L; 1L; 2L|] [|3L; 4L; 5L|]

let _ = test ()
