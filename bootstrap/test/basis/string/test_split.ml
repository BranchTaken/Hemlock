open! Basis.Rudiments
open! Basis
open String

let test () =
  let test_split s f cp = begin
    let ls1, ls2 = lsplit2_hlt s ~on:cp in
    let rs1, rs2 = rsplit2_hlt s ~on:cp in
    File.Fmt.stdout
    |> Basis.Fmt.fmt "split "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> List.pp String.pp (split s ~f)
    |> Basis.Fmt.fmt "\nsplit_rev "
    |> pp s
    |> Basis.Fmt.fmt " -> "
    |> List.pp String.pp (split_rev s ~f)
    |> Basis.Fmt.fmt "\nlsplit2_hlt "
    |> pp s
    |> Basis.Fmt.fmt " -> ("
    |> pp ls1
    |> Basis.Fmt.fmt ", "
    |> pp ls2
    |> Basis.Fmt.fmt ")\nrsplit2_hlt "
    |> pp s
    |> Basis.Fmt.fmt " -> ("
    |> pp rs1
    |> Basis.Fmt.fmt ", "
    |> pp rs2
    |> Basis.Fmt.fmt ")\n"
    |> ignore
  end in
  test_split ";a::bc;de;" (fun cp -> Codepoint.(cp = (kv (Int64.of_int (Char.code ':')))))
    (Codepoint.kv 0x3aL);
  test_split ":a::bc;de:" (fun cp -> Codepoint.(cp = (kv (Int64.of_int (Char.code ':')))))
    (Codepoint.kv 0x3bL);
  test_split ":a::bc;de;" (fun cp ->
    match Codepoint.extend_to_uns cp with
    | 0x3aL (* : *)
    | 0x3bL (* ; *) -> true
    | _ -> false
  ) (Codepoint.kv 0x3bL)

let _ = test ()
