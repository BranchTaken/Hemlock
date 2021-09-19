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
    (3., Sint.kv (-3));
    (-1., Sint.kv 61);
    (1., Sint.kv 61);
    (2., Sint.kv (-1));
    (2., Sint.kv 0);
    (2., Sint.kv 1);
    (2., Sint.kv 2);
    (2., Sint.kv 61);
    (10., Sint.kv 7);
    ((ex 1.), Sint.kv (-1));
    ((ex 1.), Sint.kv 0);
    ((ex 1.), Sint.kv 1);
    ((ex 1.), Sint.kv 2);
  ];
  printf "@]"

let _ = test ()
