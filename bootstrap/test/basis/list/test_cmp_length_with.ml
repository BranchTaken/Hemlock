open! Basis.Rudiments
open! Basis
open List

let test () =
  let test_cmp_length_with lst limit = begin
    File.Fmt.stdout
    |> Fmt.fmt " (limit="
    |> Uns.pp limit
    |> Fmt.fmt " -> "
    |> Cmp.pp (cmp_length_with lst limit)
    |> Fmt.fmt ")"
    |> ignore
  end in
  let rec test_with_lists lists = begin
    match lists with
    | [] -> ()
    | list :: lists' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "cmp_length_with "
        |> (pp Uns.pp) list
        |> ignore;
        Range.iter (0L =:< 4L) ~f:(fun limit ->
          File.Fmt.stdout
          |> Fmt.fmt (if limit = 0L then ": " else ", ")
          |> ignore;
          test_cmp_length_with list limit;
        );
        File.Fmt.stdout |> Fmt.fmt "\n" |> ignore;
        test_with_lists lists'
      end
  end in
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L]
  ] in
  test_with_lists lists

let _ = test ()
