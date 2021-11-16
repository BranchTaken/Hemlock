open! Basis.Rudiments
open! Basis
open String

let test () =
  let strs = [
    "";
    "a";
    "aa";
    "ab";
    "aa";
    "a";
    "";
  ] in
  let rec fn s strs = begin
    match strs with
    | [] -> ()
    | hd :: tl -> begin
        let () = List.iter strs ~f:(fun s2 ->
          File.Fmt.stdout
          |> Basis.Fmt.fmt "cmp "
          |> pp s
          |> Basis.Fmt.fmt " "
          |> pp s2
          |> Basis.Fmt.fmt " -> "
          |> Cmp.pp (cmp s s2)
          |> Basis.Fmt.fmt "\n"
          |> ignore
        ) in
        fn hd tl
      end
  end in
  let hd, tl = match strs with
    | hd :: tl -> hd, tl
    | [] -> not_reached ()
  in
  fn hd tl

let _ = test ()
