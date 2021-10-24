open! Basis.Rudiments
open! Basis
open Array

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
          let _ =
            File.Fmt.stdout
            |> Fmt.fmt "cmp "
            |> (fmt Uns.fmt) arr
            |> Fmt.fmt " "
            |> (fmt Uns.fmt) arr2
            |> Fmt.fmt " -> "
            |> Cmp.fmt (cmp Uns.cmp arr arr2)
            |> Fmt.fmt "\n"
          in
          ()
        ) in
        fn hd tl
      end
  end in
  let hd, tl = match arrs with
    | hd :: tl -> hd, tl
    | [] -> not_reached ()
  in
  fn hd tl

let _ = test ()
