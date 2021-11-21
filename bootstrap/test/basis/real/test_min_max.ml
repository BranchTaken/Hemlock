open! Basis.Rudiments
open! Basis
open Real

let test () =
  RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 1L)) ~f:(fun i ->
    let t0 = of_sint i in
    RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 1L)) ~f:(fun j ->
      let t1 = of_sint j in
      File.Fmt.stdout
      |> Fmt.fmt "min max "
      |> pp t0
      |> Fmt.fmt " "
      |> pp t1
      |> Fmt.fmt " -> "
      |> pp (min t0 t1)
      |> Fmt.fmt " "
      |> pp (max t0 t1)
      |> Fmt.fmt "\n"
      |> ignore
    );
  )

let _ = test ()
