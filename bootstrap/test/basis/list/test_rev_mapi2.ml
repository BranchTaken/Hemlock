open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);
    ([10L], [100L]);
    ([10L; 20L], [100L; 200L]);
    ([10L; 20L; 30L], [100L; 200L; 300L]);
  ] in
  let f i a b = (b + a + i + 1L) in
  iter list_pairs ~f:(fun (a, b) ->
    File.Fmt.stdout
    |> Fmt.fmt "    mapi2 "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (mapi2 a b ~f)
    |> Fmt.fmt "\nrev_mapi2 "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (rev_mapi2 a b ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
