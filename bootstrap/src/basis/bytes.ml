open Rudiments0

type t = byte array

module ArraySeq = struct
  module T = struct
    type t = {
      slice: String.B.Slice.t;
      bindex: uns;
    }
    type elm = byte

    let init slice =
      {slice=String.C.Slice.to_bslice slice; bindex=0L}

    let length t =
      String.B.Slice.length t.slice - t.bindex

    let next t =
      assert (length t > 0L);
      let b = String.B.Slice.get t.bindex t.slice in
      let t' = {t with bindex=Uns.succ t.bindex} in
      b, t'
  end
  include T
  include Array.Seq.MakeMono(T)
end

module Cursor = struct
  module T = struct
    type container = byte array
    type elm = byte
    type t = {
      array: container;
      index: uns;
    }

    let cmp t0 t1 =
      (* == is excessively vague in OCaml. *)
      assert ((t0.array == t1.array) || (Stdlib.( = ) t0.array t1.array));
      Uns.cmp t0.index t1.index

    let hd array =
      {array; index=0L}

    let tl array =
      {array; index=(Array.length array)}

    let seek i t =
      match Sint.(i < 0L) with
      | true -> begin
          match (Uns.bits_of_sint Sint.(neg i)) > t.index with
          | true -> halt "Cannot seek before beginning of array"
          | false -> {t with index=(t.index - Uns.(bits_of_sint (Sint.neg i)))}
        end
      | false -> begin
          match (t.index + (Uns.bits_of_sint i)) > (Array.length t.array) with
          | true -> halt "Cannot seek past end of array"
          | false -> {t with index=(t.index + (Uns.bits_of_sint i))}
        end

    let pred t =
      seek (Sint.kv (-1L)) t

    let succ t =
      seek (Sint.kv 1L) t

    let lget t =
      Array.get (Uns.pred t.index) t.array

    let rget t =
      Array.get t.index t.array

    let prev t =
      lget t, pred t

    let next t =
      rget t, succ t

    let container t =
      t.array

    let index t =
      t.index
  end
  include T
  include Cmpable.Make(T)
end

module CodepointSeq = struct
  module T = struct
    type t = {
      cursor: Cursor.t;
      past: Cursor.t;
    }

    let init ~cursor ~past =
      {cursor; past}

    let length t =
      (Cursor.index t.past) - (Cursor.index t.cursor)

    let next t =
      match (length t) = 0L with
      | true -> None
      | false -> begin
          let b, cursor' = Cursor.next t.cursor in
          let t' = {t with cursor=cursor'} in
          Some (b, t')
        end
  end
  include T
  include Codepoint.Seq.Make(T)
end

(** Action to take if a UTF-8 encoding error is encountered in [transcode]. *)
type on_invalid =
  | Error   (* Return [None]. *)
  | Replace (* Replace with '�'. *)
  | Halt    (* Halt. *)

module StringSeq = struct
  module T = struct
    type t = {
      on_invalid: on_invalid;
      seq: CodepointSeq.t;
      (* vlength is how long bytes would be if all encoding errors were corrected via replacement.
      *)
      vlength: uns;
      (* vindex tracks how many bytes would be required to correctly encode already-consumed bytes.
      *)
      vindex: uns
    }

    let rec vlength ~on_invalid seq vindex =
      match on_invalid, (CodepointSeq.to_codepoint seq) with
      | _, Some (Valid (cp, seq')) -> begin
          let vindex' = vindex + (Codepoint.Utf8.length_of_codepoint cp) in
          vlength ~on_invalid seq' vindex'
        end
      | Error, Some (Invalid _) -> None
      | Replace, Some (Invalid seq') -> begin
          let cp = Codepoint.replacement in
          let vindex' = vindex + (Codepoint.Utf8.length_of_codepoint cp) in
          vlength ~on_invalid seq' vindex'
        end
      | Halt, Some (Invalid _) -> halt "Invalid utf8 sequence"
      | _, None -> Some vindex

    let init ~on_invalid ~cursor ~past =
      let seq = CodepointSeq.init ~cursor ~past in
      match vlength ~on_invalid seq 0L with
      | Some vlength -> Some {on_invalid; seq; vlength; vindex=0L}
      | None -> None

    let length t =
      t.vlength - t.vindex

    let next t =
      match t.on_invalid, (CodepointSeq.to_codepoint t.seq) with
      | _, Some (Valid (cp, seq')) -> begin
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vindex' = t.vindex + vincr in
          let t' = {t with seq=seq'; vindex=vindex'} in
          cp, t'
        end
      | Error, Some (Invalid _) -> not_reached ()
      | Replace, Some (Invalid seq') -> begin
          let cp = Codepoint.replacement in
          let vincr = Codepoint.Utf8.length_of_codepoint cp in
          let vindex' = t.vindex + vincr in
          let t' = {t with seq=seq'; vindex=vindex'} in
          cp, t'
        end
      | Halt, Some (Invalid _)
      | _, None -> not_reached ()
  end
  include T
  include String.Seq.Codepoint.Make(T)
end

module Slice = struct
  include Slice.MakeMonoIndex(Cursor)

  let length t =
    (Cursor.index (past t)) - (Cursor.index (base t))

  let get i t =
    Array.get (Cursor.index (base t) + i) (container t)

  let xpp xppf t =
    let open Format in
    fprintf xppf "@[<h>[|";
    let rec fn xppf cursor past = begin
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          fprintf xppf ";@ %a" Byte.xpp_x elm;
          fn xppf cursor' past
        end
      | false -> ()
    end in
    let cursor, past = cursors t in
    begin
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          fprintf xppf "%a" Byte.xpp_x elm;
          fn xppf cursor' past
        end
      | false -> ()
    end;
    fprintf xppf "|]@]"

  (* XXX Use Array.Slice.fmt. *)
  let pp t formatter =
    let rec fn cursor past formatter = begin
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          formatter
          |> Fmt.fmt "; "
          |> Byte.fmt ~base:Fmt.Hex elm
          |> fn cursor' past
        end
      | false -> formatter
    end in
    formatter
    |> Fmt.fmt "[|"
    |> (fun formatter ->
      let cursor, past = cursors t in
      match Cursor.(cursor < past) with
      | true -> begin
          let elm, cursor' = Cursor.next cursor in
          formatter
          |> Byte.fmt ~base:Fmt.Hex elm
          |> fn cursor' past
        end
      | false -> formatter
    )
    |> Fmt.fmt "|]"

  let hash_fold t state =
    Hash.State.Gen.init state
    |> Hash.State.Gen.fold_u8 (length t) ~f:(fun i ->
      (Byte.extend_to_uns (Cursor.(rget (seek (Uns.bits_to_sint i) (base t))))))
    |> Hash.State.Gen.fini
    |> Uns.hash_fold (length t)

  let to_bytes slice =
    let bytes = container slice in
    Array.init (range slice) ~f:(fun i -> Array.get i bytes)

  let of_codepoint cp =
    init (Array.of_list (Codepoint.to_bytes cp))

  let of_string_slice slice =
    init (ArraySeq.to_array (ArraySeq.init slice))

  let transcode ?(on_invalid=Error) t =
    let cursor, past = cursors t in
    match StringSeq.init ~on_invalid ~cursor ~past with
    | Some seq -> Some (StringSeq.to_string seq)
    | None -> None

  let to_string t =
    transcode t

  let to_string_replace t =
    match transcode ~on_invalid:Replace t with
    | Some string -> string
    | None -> not_reached ()

  let to_string_hlt t =
    match transcode ~on_invalid:Halt t with
    | Some  string -> string
    | None -> not_reached ()
end

let xpp xppf t =
  Slice.(xpp xppf (init t))

let pp t formatter =
  formatter |> Slice.(pp (init t))

let hash_fold t state =
  Slice.(hash_fold (init t) state)

let length t =
  Slice.(length (init t))

let get i t =
  Slice.(get i (init t))

let of_codepoint cp =
  Slice.(to_bytes (of_codepoint cp))

let of_string_slice slice =
  Slice.(to_bytes (of_string_slice slice))

let to_string t =
  Slice.(to_string (init t))

let to_string_replace t =
  Slice.(to_string_replace (init t))

let to_string_hlt t =
  Slice.(to_string_hlt (init t))
