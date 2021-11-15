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
    [0L; 1L; 2L; 3L; 4L];
  ] in
  iter lists ~f:(fun l ->
    let f i _ = (i % 2L = 0L) in
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]filteri "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (filteri l ~f)
    |> Fmt.fmt " / "
    |> (pp Uns.pp) (rev_filteri l ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
