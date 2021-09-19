open! Basis.Rudiments
open! Basis
open String
open Format

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
          printf "cmp %a %a -> %a\n" pp s pp s2 Cmp.pp (cmp s s2)
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
