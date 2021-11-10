open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test arr x = begin
    let arr_x = append x arr in
    let x_arr = prepend x arr in
    File.Fmt.stdout
    |> fmt Uns.fmt arr
    |> Fmt.fmt " "
    |> Uns.fmt x
    |> Fmt.fmt ": append -> "
    |> fmt Uns.fmt arr_x
    |> Fmt.fmt ", prepend -> "
    |> fmt Uns.fmt x_arr
    |> Fmt.fmt "\n"
    |> ignore

  end in
  test [||] 0L;
  test [|0L|] 1L;
  test [|0L; 1L|] 2L;
  test [|0L; 1L; 2L|] 3L

let _ = test ()
