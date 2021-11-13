open RudimentsFunctions

type 'a synced =
  | To_string of string
  | Synced of 'a

module type Formatter = sig
  type t

  val state: t
  val fmt: string -> t -> t
  val sync: t -> t synced
end

type just =
  | Left
  | Center
  | Right

type sign =
  | Implicit
  | Explicit
  | Space

type base =
  | Bin
  | Oct
  | Dec
  | Hex

type notation =
  | Normalized
  | RadixPoint
  | Compact

let pad_default = " "
let just_default = Right
let sign_default = Implicit
let alt_default = false
let zpad_default = false
let width_default = 0L
let precision_default = 2L
let base_default = Dec
let notation_default = Compact
let pretty_default = false

let clength s =
  Stdlib.String.fold_left (fun n c ->
    match Stdlib.(Char.code c < 0b1100_0000) with
    | true -> n + 1
    | false -> n
  ) 0 s

let padding n pad =
  let padlen = Stdlib.String.length pad in
  Stdlib.String.init (n * padlen) (fun i -> Stdlib.String.get pad (i mod padlen))

let fmt ?(pad=pad_default) ?(just=just_default) ?(width=width_default) s
  ((module T): (module Formatter)) : (module Formatter) =
  (module struct
    type t = T.t
    let state = T.state |> T.fmt (
      match Stdlib.(Int64.(unsigned_compare width (of_int (clength s))) <= 0) with
      | true -> s
      | _ -> begin
          let npad = Stdlib.(Int64.(to_int width) - (clength s)) in
          match just with
          | Left -> s ^ (padding npad pad)
          | Center -> begin
              let lpad = npad / 2 in
              let rpad = npad - lpad in
              (padding lpad pad) ^ s ^ (padding rpad pad)
            end
          | Right -> (padding npad pad) ^ s
        end
    )
    let fmt = T.fmt
    let sync = T.sync
  end)

let sync ((module T): (module Formatter)) : (module Formatter) synced =
  match T.sync T.state with
  | To_string s -> To_string s
  | Synced state -> begin
      Synced (module struct
        type t = T.t
        let state = state
        let fmt = T.fmt
        let sync = T.sync
      end)
    end

let flush t =
  match sync t with
  | To_string _ -> halt "sync produced (To_string string)"
  | Synced t' -> t'

let to_string t =
  match sync t with
  | To_string s -> s
  | Synced _ -> halt "sync produced (Synced t')"
