open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);
    ([10L], [100L]);
    ([10L; 20L], [100L; 200L]);
    ([10L; 20L; 30L], [100L; 200L; 300L]);
  ] in
  let f i accum a b = begin
    let sum = (b + a + i + 1L) in
    (accum + sum), sum
  end in
  iter list_pairs ~f:(fun (a, b) ->
    let accum, c = foldi2_map a b ~init:0L ~f in
    File.Fmt.stdout
    |> Fmt.fmt "    foldi2_map "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> Uns.pp accum
    |> Fmt.fmt ", "
    |> (pp Uns.pp) c
    |> Fmt.fmt "\n"
    |> ignore;

    let accum, c = rev_foldi2_map a b ~init:0L ~f in
    File.Fmt.stdout
    |> Fmt.fmt "rev_foldi2_map "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> Uns.pp accum
    |> Fmt.fmt ", "
    |> (pp Uns.pp) c
    |> Fmt.fmt "\n"
    |> ignore;
  )

let _ = test ()
