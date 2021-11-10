open Rudiments0

type ('a, 'b) t =
  | Ok of 'a
  | Error of 'b

let xpp xpp_a xpp_b xppf = function
  | Ok a -> Format.fprintf xppf "@[<h>Ok@ %a@]" xpp_a a
  | Error b -> Format.fprintf xppf "@[<h>Error@ %a@]" xpp_b b

let pp pp_a pp_b t formatter =
  match t with
  | Ok a -> formatter |> Fmt.fmt "Ok " |> pp_a a
  | Error b -> formatter |> Fmt.fmt "Error " |> pp_b b

let ok_if b ~error =
  match b with
  | false -> Error error
  | true -> Ok ()

let error_if b ~ok =
  match b with
  | false -> Ok ok
  | true -> Error ()

let is_ok = function
  | Ok _ -> true
  | Error _ -> false

let is_error = function
  | Ok _ -> false
  | Error _ -> true

let ok_opt = function
  | Ok a -> Some a
  | Error _ -> None

let ok_hlt = function
  | Ok a -> a
  | Error _ -> halt "Not Ok"

let error_opt = function
  | Ok _ -> None
  | Error b -> Some b

let error_hlt = function
  | Ok _ -> halt "Not Error"
  | Error b -> b

let oks_errors ts =
  let rec fn ts = begin
    match ts with
    | [] -> [], []
    | Ok ok :: ts' -> begin
        let oks, errors = fn ts' in
        (ok :: oks), errors
      end
    | Error error :: ts' -> begin
        let oks, errors = fn ts' in
        oks, (error :: errors)
      end
  end in
  fn ts

let all ts =
  let oks, errors = oks_errors ts in
  match oks, errors with
  | _, _ :: _ -> Error errors
  | _, _ -> Ok oks

let all_hlt ts =
  match all ts with
  | Ok oks -> oks
  | Error _ -> halt "Not all Ok"

let ok_ignore ts =
  match all ts with
  | Ok _ -> Ok ()
  | Error errors -> Error errors

let ok_ignore_hlt ts =
  let _ = all_hlt ts in
  ()

let error_ignore ts =
  match all ts with
  | Ok oks -> Ok oks
  | Error _ -> Error ()

let error_ignore_hlt ts =
  match error_ignore ts with
  | Ok _ -> halt "Not all Error"
  | Error _ -> ()

let map_ok ~f t =
  match t with
  | Ok ok -> Ok (f ok)
  | Error error -> Error error

let map_error ~f t =
  match t with
  | Ok ok -> Ok ok
  | Error error -> Error (f error)

let merge ~ok ~error t0 t1 =
  match t0, t1 with
  | Ok ok0, Ok ok1 -> Ok (ok ok0 ok1)
  | Error error, Ok _
  | Ok _, Error error -> Error error
  | Error error0, Error error1 -> Error (error error0 error1)
