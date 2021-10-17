open! Basis.Rudiments
open! Basis
open String
open Format

let test () =
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    let rec fn i = begin
      match Uns.(i = (B.length s)) with
      | true -> ()
      | false -> begin
          printf " %a" Byte.pp_x (B.get i s);
          fn (Uns.succ i)
        end
    end in
    printf "s=%a:" pp s;
    fn 0L;
    printf "\n";
  )

let _ = test ()
