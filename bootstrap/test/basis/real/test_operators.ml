open! Basis.Rudiments
open! Basis
open Real

let test () =
  let norm_nan t = if (is_nan t) then nan else t in
  RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 2L)) ~f:(fun i ->
    let t0 = of_sint i in
    RangeF.Sint.(iter Sint.(kv (-1L) =:= kv 2L)) ~f:(fun j ->
      let t1 = of_sint j in
      File.Fmt.stdout
      |> Fmt.fmt "+ - * / %% ** copysign "
      |> pp t0
      |> Fmt.fmt " "
      |> pp t1
      |> Fmt.fmt " -> "
      |> pp (t0 + t1)
      |> Fmt.fmt " "
      |> pp (t0 - t1)
      |> Fmt.fmt " "
      |> pp (t0 * t1)
      |> Fmt.fmt " "
      |> pp (norm_nan (t0 / t1))
      |> Fmt.fmt " "
      |> pp (norm_nan (t0 % t1))
      |> Fmt.fmt " "
      |> pp (t0 ** t1)
      |> Fmt.fmt " "
      |> pp (copysign ~sign:t1 t0)
      |> Fmt.fmt "\n"
      |> ignore
    );
    File.Fmt.stdout
    |> Fmt.fmt "~- ~+ neg abs "
    |> pp t0
    |> Fmt.fmt " -> "
    |> pp (~- t0)
    |> Fmt.fmt " "
    |> pp (~+ t0)
    |> Fmt.fmt " "
    |> pp (neg t0)
    |> Fmt.fmt " "
    |> pp (abs t0)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
