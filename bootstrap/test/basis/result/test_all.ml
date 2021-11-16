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
    |> Fmt.fmt "all "
    |> (List.pp (pp String.pp String.pp)) results
    |> Fmt.fmt " -> "
    |> (pp (List.pp String.pp) (List.pp String.pp)) (all results)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
