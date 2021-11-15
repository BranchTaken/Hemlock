open! Basis.Rudiments
open! Basis
open Array

let test () =
  let print_uns_arrays arr0 arr1 = begin
    File.Fmt.stdout
    |> Fmt.fmt "[|"
    |> ignore;
    iteri2 arr0 arr1 ~f:(fun i elm0 elm1 ->
      File.Fmt.stdout
      |> Fmt.fmt (if i > 0L then "; " else "")
      |> Fmt.fmt "("
      |> Uns.pp elm0
      |> Fmt.fmt ", "
      |> Uns.pp elm1
      |> Fmt.fmt ")"
      |> ignore
    );
    File.Fmt.stdout
    |> Fmt.fmt "|]"
    |> ignore
  end in
  let test_iter2 arr0 arr1 = begin
    print_uns_arrays arr0 arr1;
    File.Fmt.stdout
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_iter2 [||] [||];
  test_iter2 [|0L|] [|1L|];
  test_iter2 [|0L; 1L|] [|2L; 3L|];
  test_iter2 [|0L; 1L; 2L|] [|3L; 4L; 5L|]

let _ = test ()
