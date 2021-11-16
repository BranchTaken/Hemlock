open! Basis.Rudiments
open! Basis
open String

let test () =
  let rec test strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        File.Fmt.stdout
        |> Basis.Fmt.fmt "hash_fold "
        |> pp s
        |> Basis.Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold s Hash.State.empty))
        |> Basis.Fmt.fmt "\n"
        |> ignore;
        test strs'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3 implementation. *)
  let strings = [""; "hello"; "hello_goodbye"; "<_>"; "«»"; "‡"; "𐆗"] in
  test strings

let _ = test ()
