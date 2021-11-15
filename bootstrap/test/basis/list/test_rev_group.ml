open! Basis.Rudiments
open! Basis
open List

let test () =
  let lists = [
    [];
    [0L];
    [0L; 1L];

    [0L; 0L];

    [0L; 0L; 0L];

    [0L; 0L; 1L; 1L];
    [0L; 1L; 1L; 2L; 2L; 3L];

    [0L; 0L; 0L; 0L];
  ] in
  let eq x0 x1 = (x0 = x1) in
  iter lists ~f:(fun l ->
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]group "
    |> (pp Uns.pp) l
    |> Fmt.fmt " ~break:eq -> "
    |> (pp (pp Uns.pp)) (group l ~break:eq)
    |> Fmt.fmt " / "
    |> (pp (pp Uns.pp)) (rev_group l ~break:eq)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
