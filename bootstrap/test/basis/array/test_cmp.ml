open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let arrs = [
    [||];
    [|0L|];
    [|0L; 0L|];
    [|0L; 1L|];
    [|0L; 0L|];
    [|0L|];
    [||];
  ] in
  let rec fn arr arrs = begin
    match arrs with
    | [] -> ()
    | hd :: tl -> begin
        let () = List.iter arrs ~f:(fun arr2 ->
          printf "cmp %a %a -> %a\n"
            (pp Uns.pp) arr
            (pp Uns.pp) arr2
            Cmp.pp (cmp Uns.cmp arr arr2)
        ) in
        fn hd tl
      end
  end in
  let hd, tl = match arrs with
    | hd :: tl -> hd, tl
    | [] -> not_reached ()
  in
  printf "@[<h>";
  fn hd tl;
  printf "@]"

let _ = test ()
