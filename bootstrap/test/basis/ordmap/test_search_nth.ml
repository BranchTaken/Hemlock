open! Basis.Rudiments
open! Basis
open! OrdmapTest
open Ordmap

let test () =
  let test_search ordmap (key_max:uns) = begin
    File.Fmt.stdout
    |> (fmt Uns.pp) ordmap
    |> Fmt.fmt "\n"
    |> ignore;
    Range.iter (0L =:< (succ key_max)) ~f:(fun probe ->
      let open Cmp in
      File.Fmt.stdout
      |> Fmt.fmt "  "
      |> Uns.pp probe
      |> Fmt.fmt " -> "
      |> (fun formatter ->
        match psearch probe ordmap with
        | None -> formatter |> Fmt.fmt "<"
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        (match search probe ordmap with
          | None -> formatter |> Fmt.fmt "<>"
          | Some i -> formatter |> Fmt.fmt "=" |> (pp_kv_pair Uns.pp) (nth i ordmap)
        )
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        match nsearch probe ordmap with
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]="
          |> (pp_kv_pair Uns.pp) (nth i ordmap)
        | None -> formatter |> Fmt.fmt ">"
      )
      |> Fmt.fmt "\n"
      |> ignore
    );
  end in
  Range.iter (0L =:< 4L) ~f:(fun len ->
    let ordmap = of_array (module Uns)
      (Array.init (0L =:< len) ~f:(fun i -> let k = (i * 2L + 1L) in k, k * 10L)) in
    let key_max = len * 2L in
    test_search ordmap key_max
  )

let _ = test ()
