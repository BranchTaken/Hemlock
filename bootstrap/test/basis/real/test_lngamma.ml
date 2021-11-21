open! Basis.Rudiments
open! Basis
open Real

let test () =
  RangeF.Sint.(iter Sint.(kv 1L =:= kv 40L)) ~f:(fun n ->
    let x = (of_sint n) / 4. in
    File.Fmt.stdout
    |> Fmt.fmt "lngamma "
    |> fmt ~pmode:Fmt.Fixed ~precision:2L ~notation:Fmt.RadixPoint x
    |> Fmt.fmt " -> "
    |> fmt ~pmode:Fmt.Fixed ~precision:9L ~notation:Fmt.RadixPoint (lngamma x)
    |> Fmt.fmt "\n"
    |> ignore
  );

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "lngamma "
        |> pp x
        |> Fmt.fmt " -> "
        |> fmt ~pmode:Fmt.Fixed ~precision:5L ~notation:Fmt.Normalized (lngamma x)
        |> Fmt.fmt "\n"
        |> ignore;
        fn xs'
      end
  end in
  fn [neg_inf; -1.; -0.; 0.; inf; nan]

let _ = test ()
