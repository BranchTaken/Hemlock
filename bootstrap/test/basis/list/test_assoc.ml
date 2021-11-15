open! Basis.Rudiments
open! Basis
open List

let test () =
  let assocs = [
    [];
    [(0L, 10L)];
    [(0L, 10L); (1L, 11L)];

    [(0L, 10L); (0L, 11L); (1L, 12L)];
    [(0L, 10L); (1L, 11L); (0L, 12L)];
    [(1L, 10L); (0L, 11L); (0L, 12L)];
    [(0L, 10L); (1L, 11L); (1L, 12L); (2L, 13L)];
  ] in
  let pp_assoc (a, b) formatter = begin
    formatter
    |> Fmt.fmt "("
    |> Uns.pp a
    |> Fmt.fmt ", "
    |> Uns.pp b
    |> Fmt.fmt ")"
  end in
  let missing = 3L in
  let cmp = Uns.cmp in
  iter assocs ~f:(fun assoc ->
    File.Fmt.stdout
    |> (pp pp_assoc) assoc
    |> Fmt.fmt "\n"
    |> ignore;
    iter assoc ~f:(fun (k, _) ->
      File.Fmt.stdout
      |> Fmt.fmt "find_hlt/mem "
      |> Uns.pp k
      |> Fmt.fmt " -> "
      |> Uns.pp (Assoc.find_hlt k ~cmp assoc)
      |> Fmt.fmt " / "
      |> Bool.pp (Assoc.mem k ~cmp assoc)
      |> Fmt.fmt "\n"
      |> ignore
    );

    File.Fmt.stdout
    |> Fmt.fmt "find/mem "
    |> Uns.pp missing
    |> Fmt.fmt " -> "
    |> ignore;
    (match (Assoc.find missing ~cmp assoc), (Assoc.mem missing ~cmp assoc); with
        | None, b -> File.Fmt.stdout |> Fmt.fmt "None / " |> Bool.pp b |> ignore
        | Some v, b -> File.Fmt.stdout |> Uns.pp v |> Fmt.fmt " / " |> Bool.pp b |> ignore
    );
    File.Fmt.stdout
    |> Fmt.fmt "\n"
    |> ignore;

    iter assoc ~f:(fun (k, _) ->
      File.Fmt.stdout
      |> Fmt.fmt "remove_hlt "
      |> Uns.pp k
      |> Fmt.fmt " -> "
      |> (pp pp_assoc) (Assoc.remove_hlt k ~cmp assoc)
      |> Fmt.fmt "\n"
      |> ignore
    );
    File.Fmt.stdout
    |> Fmt.fmt "remove "
    |> Uns.pp missing
    |> Fmt.fmt " -> "
    |> (pp pp_assoc) (Assoc.remove missing ~cmp assoc)
    |> Fmt.fmt "\nmap -> "
    |> (pp pp_assoc) (Assoc.map assoc ~f:(fun v -> v * 2L))
    |> Fmt.fmt "\ninverse -> "
    |> (pp pp_assoc) (Assoc.inverse assoc)
    |> Fmt.fmt "\n\n"
    |> ignore
  )

let _ = test ()
