open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
  ] in
  iter lists ~f:(fun l ->
    Range.Uns.iter (0L =:< (succ (length l))) ~f:(fun i ->
      let f elm = (elm >= i) in
      let l0, l1 = split_until ~f l in
      let rl0, rl1 = rev_split_until ~f l in
      File.Fmt.stdout
      |> Fmt.fmt "split_until/take_until,drop_until "
      |> (pp Uns.pp) l
      |> Fmt.fmt " ~f:(fun elm -> elm >= "
      |> Uns.pp i
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) l0
      |> Fmt.fmt " "
      |> (pp Uns.pp) l1
      |> Fmt.fmt " / "
      |> (pp Uns.pp) (take_until ~f l)
      |> Fmt.fmt " "
      |> (pp Uns.pp) (drop_until ~f l)
      |> Fmt.fmt "\nrev_split_until/rev_take_until,drop_until "
      |> (pp Uns.pp) l
      |> Fmt.fmt " ~f:(fun elm -> elm >= "
      |> Uns.pp i
      |> Fmt.fmt ") -> "
      |> (pp Uns.pp) rl0
      |> Fmt.fmt " "
      |> (pp Uns.pp) rl1
      |> Fmt.fmt " / "
      |> (pp Uns.pp) (rev_take_until ~f l)
      |> Fmt.fmt " "
      |> (pp Uns.pp) (drop_until ~f l)
      |> Fmt.fmt "\n"
      |> ignore
    )
  )

let _ = test ()
