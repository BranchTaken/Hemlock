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
    let f i elm = elm + i * 10L in
    File.Fmt.stdout
    |> Fmt.fmt "[rev_]mapi "
    |> (pp Uns.pp) l
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (mapi l ~f)
    |> Fmt.fmt " / "
    |> (pp Uns.pp) (rev_mapi l ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
