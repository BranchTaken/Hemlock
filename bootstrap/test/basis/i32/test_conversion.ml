open! Basis.Rudiments
open! Basis
open I32
open Format

let test () =
  let rec fn = function
    | [] -> ()
    | x :: xs' -> begin
        let i = x in
        let t = of_sint i in
        let i' = to_sint t in
        let t' = of_sint i' in
        printf "of_sint %a -> to_sint %a -> of_sint %a -> %a\n"
          Sint.pp_x i pp_x t Sint.pp i' pp_x t';
        let t = of_uns (Uns.of_sint i) in
        let u = to_uns t in
        let t' = of_uns u in
        printf "of_uns %a -> to_uns %a -> of_uns %a -> %a\n"
          Sint.pp_x i pp_x t Uns.pp_x u pp_x t';
        fn xs'
      end
  in
  fn [Uns.max_value; (-2L); (-1L); 0L; 1L; 2L; Uns.of_sint Sint.max_value]

let _ = test ()
