open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_fold2 uarr0 uarr1 = begin
    let _ =
      File.Fmt.stdout
      |> (fmt Uns.fmt) uarr0
      |> Fmt.fmt " "
      |> (fmt Uns.fmt) uarr1
    in
    let accum = fold2 uarr0 uarr1 ~init:0L ~f:(fun accum elm0 elm1 ->
      accum + elm0 + elm1
    ) in
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt " -> fold2 "
      |> Uns.fmt accum
    in
    let accum = foldi2 uarr0 uarr1 ~init:0L
        ~f:(fun i accum elm0 elm1 -> accum + i + elm0 + elm1 ) in
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt " -> foldi2 "
      |> Uns.fmt accum
      |> Fmt.fmt "\n"
    in
    ()
  end in
  test_fold2 [||] [||];
  test_fold2 [|1L|] [|0L|];
  test_fold2 [|3L; 2L|] [|1L; 0L|];
  test_fold2 [|5L; 4L; 3L|] [|2L; 1L; 0L|]

let _ = test ()
