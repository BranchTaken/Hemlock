open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test_search arr key_max = begin
    File.Fmt.stdout
    |> (pp Uns.pp) arr
    |> Fmt.fmt "\n"
    |> ignore;
    Range.Uns.iter (0L =:= key_max) ~f:(fun probe ->
      let open Cmp in
      File.Fmt.stdout
      |> Fmt.fmt "  "
      |> Uns.pp probe
      |> Fmt.fmt " -> "
      |> (fun formatter ->
        match psearch probe ~cmp:Uns.cmp arr with
        | None -> formatter |> Fmt.fmt "<"
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        (match search probe ~cmp:Uns.cmp arr with
          | None -> formatter |> Fmt.fmt "<>"
          | Some i -> formatter |> Fmt.fmt "=" |> Uns.pp (get i arr)
        )
      )
      |> Fmt.fmt ", "
      |> (fun formatter ->
        match nsearch probe ~cmp:Uns.cmp arr with
        | Some (Lt, i) ->
          formatter |> Fmt.fmt "<[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
        | Some (Eq, i) ->
          formatter |> Fmt.fmt "=[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
        | Some (Gt, i) ->
          formatter |> Fmt.fmt ">[" |> Uns.pp i |> Fmt.fmt "]=" |> Uns.pp (get i arr)
        | None -> formatter |> Fmt.fmt ">"
      )
      |> Fmt.fmt "\n"
      |> ignore
    )
  end in
  Range.Uns.iter (0L =:< 4L) ~f:(fun len ->
    let arr = init (0L =:< len) ~f:(fun i -> i * 2L + 1L) in
    let key_max = len * 2L in
    test_search arr key_max
  );
  Range.Uns.iter (1L =:< 4L) ~f:(fun hlen ->
    let len = hlen * 2L in
    let arr = init (0L =:< len) ~f:(fun i -> i + ((i + 1L) % 2L)) in
    let key_max = len in
    test_search arr key_max
  )

let _ = test ()
