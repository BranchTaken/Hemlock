open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L]
  ] in
  iter lists ~f:(fun l ->
    Range.Uns.iter (0L =:= length l) ~f:(fun i ->
      let a, b = split i l in
      File.Fmt.stdout
      |> Fmt.fmt "split/take,drop "
      |> Uns.pp i
      |> Fmt.fmt " "
      |> (pp Uns.pp) l
      |> Fmt.fmt " -> "
      |> (pp Uns.pp) a
      |> Fmt.fmt ", "
      |> (pp Uns.pp) b
      |> Fmt.fmt " / "
      |> (pp Uns.pp) (take i l)
      |> Fmt.fmt ", "
      |> (pp Uns.pp) (drop i l)
      |> Fmt.fmt "\n"
      |> ignore;

      let a, b = rev_split i l in
      File.Fmt.stdout
      |> Fmt.fmt "rev_split/rev_take,drop "
      |> Uns.pp i
      |> Fmt.fmt " "
      |> (pp Uns.pp) l
      |> Fmt.fmt " -> "
      |> (pp Uns.pp) a
      |> Fmt.fmt ", "
      |> (pp Uns.pp) b
      |> Fmt.fmt " / "
      |> (pp Uns.pp) (rev_take i l)
      |> Fmt.fmt ", "
      |> (pp Uns.pp) (drop i l)
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
