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
  (* Test on separate arrays. *)
  let test_blit2 r0 arr0 r1 arr1 = begin
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
    Slice.blit (Slice.init ~range:r0 arr0) (Slice.init ~range:r1 arr1);
    File.Fmt.stdout
    |> (fmt Uns.fmt) arr1
    |> Fmt.fmt "\n"
    |> ignore
  end in
  (* Test on single array, with potential for overlap. *)
  let test_blit1 r0 r1 arr = begin
    let _ =
      File.Fmt.stdout
      |> Fmt.fmt "blit "
      |> range_fmt r0
      |> Fmt.fmt " "
      |> range_fmt r1
      |> Fmt.fmt " "
      |> (fmt Uns.fmt) arr
      |> Fmt.fmt " -> "
    in
    Slice.blit (Slice.init ~range:r0 arr) (Slice.init ~range:r1 arr);
    File.Fmt.stdout
    |> (fmt Uns.fmt) arr
    |> Fmt.fmt "\n"
    |> ignore
  end in
  test_blit2 (0L =:< 0L) [||] (0L =:< 0L) [||];
  test_blit2 (0L =:< 1L) [|0L|] (0L =:< 1L) [|1L|];
  test_blit2 (1L =:< 2L) [|0L; 1L|] (0L =:< 1L) [|2L|];
  test_blit2 (0L =:< 1L) [|0L|] (1L =:< 2L) [|1L; 2L|];
  test_blit2 (0L =:< 2L) [|0L; 1L|] (0L =:< 2L) [|2L; 3L|];
  test_blit2 (1L =:< 3L) [|0L; 1L; 2L|] (0L =:< 2L) [|3L; 4L; 5L|];
  test_blit2 (0L =:< 3L) [|0L; 1L; 2L|] (0L =:< 3L) [|3L; 4L; 5L|];

  let arr = [|0L; 1L; 2L; 3L|] in
  test_blit1 (0L =:< 4L) (0L =:< 4L) arr;
  test_blit1 (0L =:< 3L) (0L =:< 3L) arr;
  test_blit1 (1L =:< 3L) (1L =:< 3L) arr;
  test_blit1 (1L =:< 4L) (1L =:< 4L) arr;

  let arr = [|0L; 1L; 2L; 3L|] in
  test_blit1 (0L =:< 1L) (2L =:< 3L) arr;

  let arr = [|0L; 1L; 2L; 3L|] in
  test_blit1 (2L =:< 3L) (0L =:< 1L) arr;

  let arr = [|0L; 1L; 2L; 3L|] in
  test_blit1 (0L =:< 3L) (1L =:< 4L) arr;

  let arr = [|0L; 1L; 2L; 3L|] in
  test_blit1 (1L =:< 4L) (0L =:< 3L) arr;

  let arr = [|0L; 1L; 2L; 3L; 4L|] in
  test_blit1 (1L =:< 3L) (2L =:< 4L) arr;

  let arr = [|0L; 1L; 2L; 3L; 4L|] in
  test_blit1 (2L =:< 4L) (1L =:< 3L) arr

let _ = test ()
