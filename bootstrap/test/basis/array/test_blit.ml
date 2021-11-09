open! Basis.Rudiments
open! Basis
open Array

let test () =
  let range_fmt r formatter =
    formatter
    |> Fmt.fmt "("
    |> Uns.fmt (Range.base r)
    |> Fmt.fmt " .. "
    |> Uns.fmt (Range.past r)
    |> Fmt.fmt ")"
  in
  let test_blit r0 arr0 r1 arr1 = begin
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt "blit "
      |> range_fmt r0
      |> Fmt.fmt " "
      |> (fmt Uns.fmt) arr0
      |> Fmt.fmt " "
      |> range_fmt r1
      |> Fmt.fmt " "
      |> (fmt Uns.fmt) arr1
      |> Fmt.fmt " -> "
    in
    blit r0 arr0 r1 arr1;
    let _ =
      File.Fmt.stdout
      |> (fmt Uns.fmt) arr1
      |> Fmt.fmt "\n"
    in
    ()
  end in
  test_blit (0L =:< 0L) [||] (0L =:< 0L) [||];
  test_blit (0L =:< 1L) [|0L|] (0L =:< 1L) [|1L|];
  test_blit (1L =:< 2L) [|0L; 1L|] (0L =:< 1L) [|2L|];
  test_blit (0L =:< 1L) [|0L|] (1L =:< 2L) [|1L; 2L|];
  test_blit (0L =:< 2L) [|0L; 1L|] (0L =:< 2L) [|2L; 3L|];
  test_blit (1L =:< 3L) [|0L; 1L; 2L|] (0L =:< 2L) [|3L; 4L; 5L|];
  test_blit (0L =:< 3L) [|0L; 1L; 2L|] (0L =:< 3L) [|3L; 4L; 5L|]

let _ = test ()
