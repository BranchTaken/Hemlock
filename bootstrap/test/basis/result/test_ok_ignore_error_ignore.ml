open! Basis.Rudiments
open! Basis
open Result

let test () =
  let results_lists = [
    [ (Ok "ok0"); (Ok "ok1"); (Ok "ok2")];
    [ (Ok "ok0"); (Error "error0"); (Ok "ok1"); (Error "error1"); (Ok "ok2")];
  ] in
  List.iter results_lists ~f:(fun results ->
    File.Fmt.stdout
    |> Fmt.fmt "ok_ignore "
    |> (List.pp (pp String.pp String.pp)) results
    |> Fmt.fmt " -> "
    |> (pp Unit.pp (List.pp String.pp)) (ok_ignore results)
    |> Fmt.fmt "\nerror_ignore "
    |> (List.pp (pp String.pp String.pp)) results
    |> Fmt.fmt " -> "
    |> (pp (List.pp String.pp) Unit.pp) (error_ignore results)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
