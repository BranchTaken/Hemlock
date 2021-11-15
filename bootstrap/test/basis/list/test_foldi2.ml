open! Basis.Rudiments
open! Basis
open List

let test () =
  let list_pairs = [
    ([], []);

    ([100L], [200L]);
    ([100L; 110L], [200L; 210L]);
    ([100L; 110L; 120L], [200L; 210L; 220L]);
  ] in
  let f i accum a_elm b_elm = begin
    (i + a_elm + b_elm) :: accum
  end in
  iter list_pairs ~f:(fun (a, b) ->
    File.Fmt.stdout
    |> Fmt.fmt "foldi2 "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (foldi2 a b ~init:[] ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
