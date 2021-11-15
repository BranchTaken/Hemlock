open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);
    ([0L], [1L]);
    ([0L; 1L], [2L; 3L]);
    ([0L; 1L; 2L], [3L; 4L; 5L])
  ] in
  let pp_pair (a, b) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> Uns.pp a
    |> Fmt.fmt ", "
    |> Uns.pp b
    |> Fmt.fmt ")"
  end in
  iter list_pairs ~f:(fun (t0, t1) ->
    let z = zip t0 t1 in
    let t0', t1' = unzip z in
    File.Fmt.stdout
    |> Fmt.fmt "zip/unzip "
    |> (pp Uns.pp) t0
    |> Fmt.fmt " "
    |> (pp Uns.pp) t1
    |> Fmt.fmt " -> "
    |> (pp pp_pair) z
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) t0'
    |> Fmt.fmt " "
    |> (pp Uns.pp) t1'
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
