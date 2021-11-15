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
  iter list_pairs ~f:(fun (a, b) ->
    let f i accum a_elm b_elm = begin
      let len = length a in
      let limit = len - 2L in
      ((i + a_elm + b_elm) :: accum), (i = limit)
    end in
    File.Fmt.stdout
    |> Fmt.fmt "foldi2_until "
    |> (pp Uns.pp) a
    |> Fmt.fmt " "
    |> (pp Uns.pp) b
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (foldi2_until a b ~init:[] ~f)
    |> Fmt.fmt "\n"
    |> ignore
  )

let _ = test ()
