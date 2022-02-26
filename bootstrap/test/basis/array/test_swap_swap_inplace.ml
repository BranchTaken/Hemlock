open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_swap arr = begin
    Range.Uns.iter (0L =:< length arr) ~f:(fun i ->
      Range.Uns.iter (i =:< length arr) ~f:(fun j ->
        let arr' = copy arr in
        File.Fmt.stdout
        |> Uns.pp i
        |> Fmt.fmt " "
        |> Uns.pp j
        |> Fmt.fmt ": swap "
        |> (pp Uns.pp) arr'
        |> Fmt.fmt " -> "
        |> (pp Uns.pp) (swap i j arr')
        |> Fmt.fmt " -> swap_inplace "
        |> (pp Uns.pp) arr'
        |> Fmt.fmt " -> "
        |> ignore;

        swap_inplace i j arr';
        File.Fmt.stdout
        |> (pp Uns.pp) arr'
        |> Fmt.fmt "\n"
        |> ignore
      )
    )
  end in
  test_swap [|0L|];
  test_swap [|0L; 1L|];
  test_swap [|0L; 1L; 2L|];
  test_swap [|0L; 1L; 2L; 3L|]

let _ = test ()
