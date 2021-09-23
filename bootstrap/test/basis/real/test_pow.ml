open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec fn pairs = begin
    match pairs with
    | [] -> ()
    | (b, x) :: pairs' -> begin
        let xf = of_sint x in
        printf "** pow int_pow %h ~p:%a -> %h %h %h\n"
          b Sint.pp x (b ** xf) (pow b ~p:xf) (int_pow b ~p:x);
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
  ];
  printf "@]"

let _ = test ()
