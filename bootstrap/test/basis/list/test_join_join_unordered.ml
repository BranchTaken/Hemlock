open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_lists = [
    [];

    [[]];
    [[0L; 1L]];

    [[]; []];
    [[0L; 1L]; [2L; 3L]];

    [[]; []; []];
    [[0L; 1L]; [2L; 3L]; [4L; 5L]];
  ] in
  iter list_lists ~f:(fun lists ->
    File.Fmt.stdout
    |> Fmt.fmt "join"
    |> (fun formatter ->
      fold lists ~init:formatter ~f:(fun formatter l -> formatter |> Fmt.fmt " " |> (pp Uns.pp) l)
    )
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (join lists)
    |> Fmt.fmt "\njoin ~sep:[6; 7]"
    |> (fun formatter ->
      fold lists ~init:formatter ~f:(fun formatter l -> formatter |> Fmt.fmt " " |> (pp Uns.pp) l)
    )
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (join ~sep:[6L; 7L] lists)
    (* Brittle test; change in conjunction with implementation. *)
    |> Fmt.fmt "\njoin_unordered ~sep:[6; 7]"
    |> (fun formatter ->
      fold lists ~init:formatter ~f:(fun formatter l -> formatter |> Fmt.fmt " " |> (pp Uns.pp) l)
    )
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (join_unordered ~sep:[6L; 7L] lists)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
