open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);
    ([0L], [1L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L; 2L], [3L; 4L; 5L]);
  ] in
  iter list_pairs ~f:(fun (a, b) ->
    File.Fmt.stdout
    |> Fmt.fmt "iter2 "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " ->"
    |> ignore;
    let f i a b = begin
      File.Fmt.stdout
      |> Fmt.fmt " (i="
      |> Uns.pp i
      |> Fmt.fmt ", a="
      |> Uns.pp a
      |> Fmt.fmt ", b="
      |> Uns.pp b
      |> Fmt.fmt ")"
      |> ignore
    end in
    iteri2 a b ~f;
    File.Fmt.stdout |> Fmt.fmt "\n" |> ignore
  )

let _ = test ()
