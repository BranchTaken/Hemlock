open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [9L];
    [9L; 9L];

    [0L; 1L];
    [9L; 1L; 2L; 9L];

    [0L; 1L; 2L];
    [9L; 1L; 2L; 9L; 4L; 5L; 9L];
  ] in
  let inds i x0 x1 = (i = x1) && (i = succ x0) in
  iter lists ~f:(fun l ->
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]groupi "
    |> (pp Uns.pp) l
    |> Fmt.fmt " ~break:inds -> "
    |> (pp (pp Uns.pp)) (groupi l ~break:inds)
    |> Fmt.fmt " / "
    |> (pp (pp Uns.pp)) (rev_groupi l ~break:inds)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
