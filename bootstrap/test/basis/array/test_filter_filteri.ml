open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_filter arr = begin
    let farr = filter arr ~f:(fun elm -> elm % 2L = 0L) in
    let farr2 = filteri arr ~f:(fun _ elm -> elm % 2L = 0L) in
    let farr3 = filteri arr ~f:(fun i _ -> i % 2L = 0L) in
    File.Fmt.stdout
    |> (fmt Uns.fmt) arr
    |> Fmt.fmt " -> filter "
    |> (fmt Uns.fmt) farr
    |> Fmt.fmt " -> filteri "
    |> (fmt Uns.fmt) farr2
    |> Fmt.fmt " "
    |> (fmt Uns.fmt) farr3
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_filter [||];
  test_filter [|0L|];
  test_filter [|1L; 0L|];
  test_filter [|2L; 1L; 0L|];
  test_filter [|3L; 2L; 1L; 0L|]

let _ = test ()
