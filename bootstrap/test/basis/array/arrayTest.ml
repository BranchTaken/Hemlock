open! Basis.Rudiments
open! Basis

let iter_oc base past f =
  let rec fn i past f = begin
    match Uns.(i < past) with
    | false -> ()
    | true -> begin
        f i;
        fn (Uns.succ i) past f
      end
  end in
  fn base past f
