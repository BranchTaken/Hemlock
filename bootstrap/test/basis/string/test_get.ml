open! Basis.Rudiments
open! Basis
open String

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
          File.Fmt.stdout
          |> Basis.Fmt.fmt " "
          |> Byte.fmt ~alt:true ~base:Basis.Fmt.Hex ~pretty:true (B.get i s)
          |> ignore;
          fn (Uns.succ i)
        end
    end in
    File.Fmt.stdout
    |> Basis.Fmt.fmt "s="
    |> pp s
    |> Basis.Fmt.fmt ":"
    |> ignore;
    fn 0L;
    File.Fmt.stdout |> Basis.Fmt.fmt "\n" |> ignore
  )

let _ = test ()
