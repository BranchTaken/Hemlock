open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_fold2_until uarr0 uarr1 = begin
    let _ =
      File.Fmt.stdout
      |> (fmt Uns.fmt) uarr0
      |> Fmt.fmt " "
      |> (fmt Uns.fmt) uarr1
    in
    let accum = fold2_until uarr0 uarr1 ~init:0L
        ~f:(fun accum elm0 elm1 ->
          (accum + elm0 + elm1), (accum > 10L)
        ) in
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt " -> fold2_until "
      |> Uns.fmt accum
    in
    let accum = foldi2_until uarr0 uarr1 ~init:0L
        ~f:(fun i accum elm0 elm1 ->
          (accum + i + elm0 + elm1), ((i + 2L) >= (length uarr0))
        ) in
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt " -> foldi2_until "
      |> Uns.fmt accum
      |> Fmt.fmt "\n"
    in
    ()
  end in
  test_fold2_until [||] [||];
  test_fold2_until [|1L|] [|0L|];
  test_fold2_until [|3L; 2L|] [|1L; 0L|];
  test_fold2_until [|5L; 4L; 3L|] [|2L; 1L; 0L|]

let _ = test ()
