open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (b, x) :: pairs' -> begin
        let xf = of_sint x in
        File.Fmt.stdout
        |> Fmt.fmt "** pow "
        |> fmt ~alt:true ~radix:Radix.Hex b
        |> Fmt.fmt " ~p:"
        |> Sint.pp x
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~radix:Radix.Hex (b ** xf)
        |> Fmt.fmt " "
        |> fmt ~alt:true ~radix:Radix.Hex (pow b ~p:xf)
        |> Fmt.fmt "\n"
        |> ignore;
        fn pairs'
      end
  end in
  fn [
    (3., Sint.kv (-3L));
    (-1., Sint.kv 61L);
    (1., Sint.kv 61L);
    (2., Sint.kv (-1L));
    (2., Sint.kv 0L);
    (2., Sint.kv 1L);
    (2., Sint.kv 2L);
    (2., Sint.kv 61L);
    (10., Sint.kv 7L);
    ((ex 1.), Sint.kv (-1L));
    ((ex 1.), Sint.kv 0L);
    ((ex 1.), Sint.kv 1L);
    ((ex 1.), Sint.kv 2L);
  ]

let _ = test ()
