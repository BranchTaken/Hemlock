open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];
    [0L; 1L; 2L];
    [0L; 1L; 2L; 3L];
  ] in
  let f i accum elm = (elm :: accum), (elm + i * 10L) in
  iter lists ~f:(fun l ->
    let accum, b_list = foldi_map l ~init:[] ~f in
    File.Fmt.stdout
    |> Fmt.fmt "    fold_mapi "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> accum="
    |> (pp Uns.pp) accum
    |> Fmt.fmt ", b_list="
    |> (pp Uns.pp) b_list
    |> Fmt.fmt "\n"
    |> ignore;

    let accum, b_list = rev_foldi_map l ~init:[] ~f in
    File.Fmt.stdout
    |> Fmt.fmt "rev_fold_mapi "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> accum="
    |> (pp Uns.pp) accum
    |> Fmt.fmt ", b_list="
    |> (pp Uns.pp) b_list
    |> Fmt.fmt "\n"
    |> ignore;
  )

let _ = test ()
