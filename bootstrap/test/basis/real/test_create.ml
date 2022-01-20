open! Basis.Rudiments
open! Basis
open Real

let test () =
  let rec fn tups = begin
    match tups with
    | [] -> ()
    | (n, e, m) :: tups' -> begin
        let f = create ~neg:n ~exponent:e ~mantissa:m in
        File.Fmt.stdout
        |> Fmt.fmt "n="
        |> Bool.pp n
        |> Fmt.fmt ", e="
        |> Sint.pp e
        |> Fmt.fmt ", m="
        |> Uns.fmt ~alt:true ~zpad:true ~width:13L ~radix:Radix.Hex m
        |> Fmt.fmt " -> "
        |> fmt ~alt:true ~radix:Radix.Hex f
        |> Fmt.fmt "\n"
        |> ignore;
        fn tups'
      end
  end in
  fn [
    (* Infinite. *)
    (true, Sint.kv 1024L, 0L);
    (false, Sint.kv 1024L, 0L);

    (* Nan. *)
    (false, Sint.kv 1024L, 1L);
    (false, Sint.kv 1024L, 0x8_0000_0000_0001L);
    (false, Sint.kv 1024L, 0xf_ffff_ffff_ffffL);

    (* Normal. *)
    (true, Sint.kv 0L, 0L);
    (false, Sint.kv (-1022L), 0L);
    (false, Sint.kv (-52L), 1L);
    (false, Sint.kv (-51L), 1L);
    (false, Sint.kv (-1L), 0L);
    (false, Sint.kv 0L, 0L);
    (false, Sint.kv 1L, 0L);
    (false, Sint.kv 1L, 0x8_0000_0000_0000L);
    (false, Sint.kv 2L, 0L);
    (false, Sint.kv 2L, 0x4_0000_0000_0000L);
    (false, Sint.kv 1023L, 0xf_ffff_ffff_ffffL);

    (* Subnormal. *)
    (false, Sint.kv (-1023L), 1L);
    (false, Sint.kv (-1023L), 0xf_ffff_ffff_ffffL);

    (* Zero. *)
    (true, Sint.kv (-1023L), 0L);
    (false, Sint.kv (-1023L), 0L);
  ]

let _ = test ()
