open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let print_uns_arrays arr0 arr1 = begin
    printf "[|";
    iteri2 arr0 arr1 ~f:(fun i elm0 elm1 ->
      if i > 0 then printf "; ";
      printf "(%a, %a)" Uns.pp elm0 Uns.pp elm1
    );
    printf "|]"
  end in
  let test_iter2 arr0 arr1 = begin
    print_uns_arrays arr0 arr1;
    printf "\n"
  end in
  test_iter2 [||] [||];
  test_iter2 [|0|] [|1|];
  test_iter2 [|0; 1|] [|2; 3|];
  test_iter2 [|0; 1; 2|] [|3; 4; 5|]

let _ = test ()
