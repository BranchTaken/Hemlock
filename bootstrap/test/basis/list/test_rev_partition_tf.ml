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
  iter lists ~f:(fun l ->
    let even x = (x % 2L = 0L) in
    let l_true, l_false = partition_tf l ~f:even in
    let rl_true, rl_false = rev_partition_tf l ~f:even in
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]partition_tf "
    |> (pp Uns.pp) l
    |> Fmt.fmt " ~f:even -> "
    |> (pp Uns.pp) l_true
    |> Fmt.fmt " "
    |> (pp Uns.pp) l_false
    |> Fmt.fmt " / "
    |> (pp Uns.pp) rl_true
    |> Fmt.fmt " "
    |> (pp Uns.pp) rl_false
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
