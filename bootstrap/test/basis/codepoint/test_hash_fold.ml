open! Basis.Rudiments
open! Basis
open Codepoint

let test () =
  let rec test_hash_fold cps = begin
    match cps with
    | [] -> ()
    | cp :: cps' -> begin
        File.Fmt.stdout
        |> Fmt.fmt "hash_fold "
        |> (Uns.fmt ~alt:true ~zpad:true ~width:16L ~base:Fmt.Hex) (extend_to_uns cp)
        |> Fmt.fmt " -> "
        |> Hash.pp (Hash.t_of_state (hash_fold cp Hash.State.empty))
        |> Fmt.fmt "\n"
        |> ignore;
        test_hash_fold cps'
      end
  end in
  let cps = [nul; soh; del; replacement; kv 0x10_ffffL] in
  test_hash_fold cps

let _ = test ()
