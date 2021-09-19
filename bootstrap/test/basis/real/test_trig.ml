open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec fn = function
    | [] -> ()
    | t :: ts' -> begin
        let sin_t = sin t in
        let cos_t = cos t in
        let tan_t = tan t in
        printf "sin cos tan %.5f -> %.5f %.5f %.5f\n" t sin_t cos_t tan_t;
        printf "asin acos atan atan2 -> %.5f %.5f %.5f %.5f\n"
          (asin sin_t) (acos cos_t) (atan tan_t) (atan2 sin_t cos_t);
        fn ts'
      end
  in
  fn [
    0.; (pi / 6.); (pi / 4.); (2./3. * pi); pi; (4./3. * pi); (2. * pi);
  ];
  printf "@]"

let _ = test ()
