open! Basis.Rudiments
open! Basis
open List

let test () =
  let test_length lst = begin
    File.Fmt.stdout
    |> Fmt.fmt "["
    |> ignore;
    Range.iter (0L =:< (length lst)) ~f:(fun i ->
      File.Fmt.stdout
      |> Fmt.fmt (if i > 0L then "; " else "")
      |> Uns.pp (nth i lst)
      |> ignore
    );
    File.Fmt.stdout
    |> Fmt.fmt "]: length="
    |> Uns.pp (length lst)
    |> Fmt.fmt ", is_empty="
    |> Bool.pp (is_empty lst)
    |> Fmt.fmt "\n"
    |> Fmt.fmt ""
    |> ignore
  end in
  test_length [];
  test_length [0L];
  test_length [0L; 1L];
  test_length [0L; 1L; 2L]

let _ = test ()
