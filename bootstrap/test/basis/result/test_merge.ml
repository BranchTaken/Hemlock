open! Basis.Rudiments
open! Basis
open Result

let test () =
  let result_pairs = [
    (Ok "ok0", Ok "ok1");
    (Error "error0", Ok "ok1");
    (Ok "ok0", Error "error1");
    (Error "error0", Error "error1")
  ] in
  List.iter result_pairs ~f:(fun (a, b) ->
    let f a b = String.Fmt.empty |> Fmt.fmt a |> Fmt.fmt " + " |> Fmt.fmt b |> Fmt.to_string in
    File.Fmt.stdout
    |> Fmt.fmt "merge ("
    |> (pp String.pp String.pp) a
    |> Fmt.fmt ") ("
    |> (pp String.pp String.pp) b
    |> Fmt.fmt ") -> ("
    |> (pp String.pp String.pp) (merge a b ~ok:f ~error:f)
    |> Fmt.fmt ")\n"
    |> ignore
  )

let _ = test ()
