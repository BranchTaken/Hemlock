open! Basis.Rudiments
open! Basis
open! Hmc
open Realer

let test () =
  let cmp_opt_pp cmp_opt formatter = begin
    match cmp_opt with
    | None -> formatter |> Fmt.fmt "NA"
    | Some rel -> formatter |> Cmp.pp rel
  end in
  let cmp_opt t0 t1 = begin
    match is_nan t0 || (is_nan t1) with
    | true -> None
    | false -> Some (cmp t0 t1)
  end in
  let rec fn = function
    | [] -> ()
    | (t0, t1) :: tups' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "+,-,*,cmp "
        |> pp t0
        |> Fmt.fmt " "
        |> pp t1
        |> Fmt.fmt "\n  -> "
        |> pp (t0 + t1)
        |> Fmt.fmt "\t"
        |> pp (t0 - t1)
        |> Fmt.fmt "\t"
        |> pp (t0 * t1)
        |> Fmt.fmt "\t"
        |> cmp_opt_pp (cmp_opt t0 t1)
        |> Fmt.fmt "\n"
        |> ignore;
        fn tups'
      end
  in
  fn [
    (zero, zero);
    (zero, one);
    (one, zero);
    (one, one);
    (zero, inf);
    (inf, zero);
    (one, inf);
    (inf, one);

    (neg zero, zero);
    (neg zero, one);
    (neg one, zero);
    (neg one, one);
    (neg zero, inf);
    (neg inf, zero);
    (neg one, inf);
    (neg inf, one);

    (zero, neg zero);
    (zero, neg one);
    (one, neg zero);
    (one, neg one);
    (zero, neg inf);
    (inf, neg zero);
    (one, neg inf);
    (inf, neg one);

    (neg zero, neg zero);
    (neg zero, neg one);
    (neg one, neg zero);
    (neg one, neg one);
    (neg zero, neg inf);
    (neg inf, neg zero);
    (neg one, neg inf);
    (neg inf, neg one);

    (zero, nan);
    (nan, zero);
    (one, nan);
    (nan, one);
    (inf, nan);
    (nan, inf);

    (create ~sign:Neg ~exponent:(Zint.of_uns 0L) ~mantissa:(Nat.of_uns 0x1f0fL),
      create ~sign:Neg ~exponent:(Zint.of_uns 8L) ~mantissa:(Nat.of_uns 0x1f0fL));
    (create ~sign:Neg ~exponent:(Zint.of_uns 0L) ~mantissa:(Nat.of_uns 0x1f0fL),
      create ~sign:Neg ~exponent:(Zint.of_uns 7L) ~mantissa:(Nat.of_uns 0x1f0fL));
  ]

let _ = test ()
