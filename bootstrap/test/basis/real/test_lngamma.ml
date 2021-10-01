open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  RangeF.Sint.(iter Sint.(kv 1L =:= kv 40L)) ~f:(fun n ->
    let x = (of_sint n) / 4. in
    printf "lngamma %.2f -> %.9f\n" x (lngamma x);
  );

  let rec fn xs = begin
    match xs with
    | [] -> ()
    | x :: xs' -> begin
        printf "lngamma %.2f -> %.5e\n" x (lngamma x);
        fn xs'
      end
  end in
  fn [neg_inf; -1.; -0.; 0.; inf; nan];
  printf "@]"

let _ = test ()
