open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let test_split s f cp = begin
    printf "split %a -> [" pp s;
    List.iteri (split s ~f) ~f:(fun i substr ->
      if Uns.(i > 0L) then printf "; ";
      printf "%a" pp substr
    );
    printf "]\n";

    printf "split_rev %a -> [" pp s;
    List.iteri (split_rev s ~f)~f:(fun i substr ->
      if Uns.(i > 0L) then printf "; ";
      printf "%a" pp substr
    );
    printf "]\n";

    let s1, s2 = lsplit2_hlt s ~on:cp in
    printf "lsplit2_hlt %a -> (%a, %a)\n" pp s pp s1 pp s2;

    let s1, s2 = rsplit2_hlt s ~on:cp in
    printf "rsplit2_hlt %a -> (%a, %a)\n" pp s pp s1 pp s2;
  end in
  test_split ";a::bc;de;" (fun cp -> Codepoint.(cp = (kv (Int64.of_int (Char.code ':')))))
    (Codepoint.kv 0x3aL);
  test_split ":a::bc;de:" (fun cp -> Codepoint.(cp = (kv (Int64.of_int (Char.code ':')))))
    (Codepoint.kv 0x3bL);
  test_split ":a::bc;de;" (fun cp ->
    match Codepoint.to_uns cp with
    | 0x3aL (* : *)
    | 0x3bL (* ; *) -> true
    | _ -> false
  ) (Codepoint.kv 0x3bL)

let _ = test ()
