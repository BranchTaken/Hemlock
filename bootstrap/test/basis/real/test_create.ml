open! Basis.Rudiments
open! Basis
open Real
open Format

let test () =
  printf "@[<h>";
  let rec fn tups = begin
    match tups with
    | [] -> ()
    | (n, e, m) :: tups' -> begin
        let f = create ~neg:n ~exponent:e ~mantissa:m in
        printf "n=%b, e=%a, m=%a -> %h -> n=%b, e=%a, m=%a\n"
          n Sint.pp e Uns.pp_x m f n Sint.pp e Uns.pp_x m;
        fn tups'
      end
  end in
  fn [
    (* Infinite. *)
    (true, Sint.kv 1024, 0);
    (false, Sint.kv 1024, 0);

    (* Nan. *)
    (false, Sint.kv 1024, 1);
    (false, Sint.kv 1024, 0x8_0000_0000_0001);
    (false, Sint.kv 1024, 0xf_ffff_ffff_ffff);

    (* Normal. *)
    (true, Sint.kv 0, 0);
    (false, Sint.kv (-1022), 0);
    (false, Sint.kv (-52), 1);
    (false, Sint.kv (-51), 1);
    (false, Sint.kv (-1), 0);
    (false, Sint.kv 0, 0);
    (false, Sint.kv 1, 0);
    (false, Sint.kv 1, 0x8_0000_0000_0000);
    (false, Sint.kv 2, 0);
    (false, Sint.kv 2, 0x4_0000_0000_0000);
    (false, Sint.kv 1023, 0xf_ffff_ffff_ffff);

    (* Subnormal. *)
    (false, Sint.kv (-1023), 1);
    (false, Sint.kv (-1023), 0xf_ffff_ffff_ffff);

    (* Zero. *)
    (true, Sint.kv (-1023), 0);
    (false, Sint.kv (-1023), 0);
  ];
  printf "@]"

let _ = test ()
