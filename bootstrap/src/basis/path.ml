open RudimentsFunctions

module Segment = struct
  type t =
    | Empty
    | Current
    | Parent
    | Sslice of String.C.Slice.t
    | Bslice of Bytes.Slice.t (* Used only if Sslice cannot be used. *)

  let of_string_slice sslice =
    match String.C.Slice.to_string sslice with
    | "" -> Empty
    | "." -> Current
    | ".." -> Parent
    | _ -> Sslice sslice

  let of_string s =
    of_string_slice (String.C.Slice.of_string s)

  let of_bytes bslice =
    match Bytes.Slice.to_string bslice with
    | None -> Bslice bslice
    | Some s -> of_string s

  let to_string_slice = function
    | Empty -> Some (String.C.Slice.of_string "")
    | Current -> Some (String.C.Slice.of_string ".")
    | Parent -> Some (String.C.Slice.of_string "..")
    | Sslice sslice -> Some sslice
    | Bslice _ -> None

  let to_string t =
    match to_string_slice t with
    | None -> None
    | Some sslice -> Some (String.C.Slice.to_string sslice)

  let to_string_slice_replace t =
    match to_string_slice t with
    | Some sslice -> sslice
    | None -> begin
        match t with
        | Empty
        | Current
        | Parent
        | Sslice _ -> not_reached ()
        | Bslice bslice -> String.C.Slice.of_string (Bytes.Slice.to_string_replace bslice)
      end

  let to_string_replace t =
    String.C.Slice.to_string (to_string_slice_replace t)

  let to_string_hlt t =
    match to_string t with
    | None -> halt "Invalid utf8 sequence"
    | Some s -> s

  let pp t formatter =
    formatter |> String.pp (to_string_replace t)

  let to_bytes t =
    match t with
    | Empty
    | Current
    | Parent
    | Sslice _ -> Bytes.Slice.of_string_slice (String.C.Slice.of_string (to_string_hlt t))
    | Bslice bslice -> bslice

  let is_empty = function
    | Empty -> true
    | _ -> false

  let is_current = function
    | Current -> true
    | _ -> false

  let is_parent = function
    | Parent -> true
    | _ -> false

  let split t =
    match t with
    | Empty -> [Empty]
    | Current -> [Empty]
    | Parent -> [Parent]
    | Sslice sslice -> begin
        let rec f accum base cursor past = begin
          match String.C.Cursor.(<) base cursor with
          | false -> begin
              match String.C.Cursor.(<) base past with
              | false -> accum
              | true -> begin
                  let stem = of_string_slice (String.C.Slice.of_cursors ~base ~past) in
                  stem :: accum
                end
            end
          | true -> begin
              let cp, cursor' = String.C.Cursor.prev cursor in
              match cp with
              | cp when Codepoint.(cp = of_char '.') -> begin
                  let suffix = of_string_slice (String.C.Slice.of_cursors ~base:cursor' ~past) in
                  let accum' = suffix :: accum in
                  f accum' base cursor' cursor'
                end
              | _ -> f accum base cursor' past
            end
        end in
        let base = String.C.Slice.base sslice in
        let past = String.C.Slice.past sslice in
        f [] base past past
      end
    | Bslice bslice -> begin
        let rec f accum base cursor past = begin
          match Bytes.Cursor.(<) base cursor with
          | false -> begin
              match Bytes.Cursor.(<) base past with
              | false -> accum
              | true -> begin
                  let stem = of_bytes (Bytes.Slice.of_cursors ~base ~past) in
                  stem :: accum
                end
            end
          | true -> begin
              let b, cursor' = Bytes.Cursor.prev cursor in
              match b with
              | b when Byte.(b = of_char '.') -> begin
                  let suffix = of_bytes (Bytes.Slice.of_cursors ~base:cursor' ~past) in
                  let accum' = suffix :: accum in
                  f accum' base cursor' cursor'
                end
              | _ -> f accum base cursor' past
            end
        end in
        let base = Bytes.Slice.base bslice in
        let past = Bytes.Slice.past bslice in
        f [] base past past
      end

  let stem t =
    List.hd (split t)

  let suffixes t =
    List.tl (split t)

  let suffix t =
    let rec f = function
      | [] -> Empty
      | t :: [] -> t
      | _ :: t' -> f t'
    in
    f (suffixes t)

  let join ts =
    (* Join segments of which none are Bslice. *)
    let join_string ts = begin
      of_string_slice (String.C.Slice.join (List.map ts ~f:(fun segment ->
        match segment with
        | Empty -> String.C.Slice.of_string ""
        | Current
        | Parent -> not_reached ()
        | Sslice sslice -> sslice
        | Bslice _ -> not_reached ()
      )))
    end in
    (* Join segments of which at least one is Bslice. *)
    let join_bytes ts = begin
      of_bytes (Bytes.Slice.join (List.map ts ~f:(fun segment ->
        match segment with
        | Empty -> Bytes.Slice.init [||]
        | Current
        | Parent -> not_reached ()
        | Sslice sslice -> Bytes.Slice.of_string_slice sslice
        | Bslice bslice -> bslice
      )))
    end in
    match ts with
    | [] -> Empty
    | t :: [] -> t
    | _ -> begin
        match List.fold ~init:false ts ~f:(fun accum t ->
          match t with
          | Empty -> accum
          | Current
          | Parent -> halt "Invalid join admixture"
          | Sslice _ -> accum
          | Bslice _ -> true
        ) with
        | false -> join_string ts
        | true -> join_bytes ts
      end
end

type t = Segment.t list

let of_string s =
  match s with
  | "" -> []
  | _ -> begin
      let sslices = String.C.Slice.split_fold (String.C.Slice.of_string s) ~init:[]
        ~on:(fun cp -> Codepoint.(cp = of_char '/')) ~f:(fun accum sslice -> sslice :: accum) in
      List.map sslices ~f:(fun s -> Segment.of_string_slice s)
    end

let of_bytes bslice =
  match Bytes.Slice.length bslice with
  | 0L -> []
  | _ -> begin
      let rec f accum base cursor past = begin
        match Bytes.Cursor.(<) cursor past with
        | false -> begin
            let slice = Bytes.Slice.of_cursors ~base ~past in
            slice :: accum
          end
        | true -> begin
            let b, cursor' = Bytes.Cursor.next cursor in
            match Byte.(b = of_char '/') with
            | true -> begin
                let slice = Bytes.Slice.of_cursors ~base ~past:cursor in
                f (slice :: accum) cursor' cursor' past
              end
            | false -> f accum base cursor' past
          end
      end in
      let base = Bytes.Slice.base bslice in
      let past = Bytes.Slice.past bslice in
      let bslices = f [] base base past in
      List.map bslices ~f:(fun bslice -> Segment.of_bytes bslice)
    end

let of_segment segment =
  [segment]

let is_abs t =
  let rec f t = begin
    let open Segment in
    match t with
    | [] -> false
    | Empty :: [] -> true
    | _ :: t' -> f t'
  end in
  f t

let is_empty = function
  | [] -> true
  | _ :: _ -> false

let split = function
  | [] -> [], None
  | (Segment.Empty :: []) as dirname -> dirname, None
  | basename :: dirname -> dirname, Some basename

let dirname t =
  match split t with dirname, _ -> dirname

let basename t =
  match split t with _, basename -> basename

let normalize t =
  let rec f t = begin
    let open Segment in
    let t' = match t with
      | []
      | Empty :: []
      | Empty :: Empty :: [] -> t
      | Empty :: Empty :: Empty :: [] -> Empty :: []
      | (Empty|Current) :: t' -> f t'
      | a :: t' -> a :: (f t')
    in
    match t' with
    | Parent :: (Sslice _|Bslice _) :: t'' -> t''
    | _ -> t'
  end in
  match split t with
  | (dirname, None) -> dirname
  | (dirname, Some basename) -> begin
      let dirname' = f dirname in
      match basename :: dirname' with
      | (Empty|Current) :: Empty :: t' -> Empty :: Empty :: t'
      | (Empty|Current) :: _ -> dirname'
      | Parent :: (Sslice _|Bslice _) :: Empty :: t' -> Empty :: Empty :: t'
      | Parent :: (Sslice _|Bslice _) :: t' -> t'
      | _ as t' -> t'
    end

let join ts =
  List.join (List.rev ts)

let to_string t =
  let sslices_opt = List.fold_until t ~init:(Some []) ~f:(fun accum segment ->
    match Segment.to_string_slice segment with
    | None -> None, true
    | Some sslice -> Some (sslice :: Option.value_hlt accum), false
  ) in
  match sslices_opt with
  | None -> None
  | Some strs -> Some String.C.Slice.(to_string (join ~sep:(of_string "/") strs))

let to_string_replace t =
  let sslices = List.rev_map t ~f:(fun segment -> Segment.to_string_slice_replace segment) in
  String.C.Slice.(to_string (join ~sep:(of_string "/") sslices))

let to_string_hlt t =
  match to_string t with
  | None -> halt "Invalid utf8 sequence"
  | Some s -> s

let to_bytes t =
  let bslices = List.rev_map t ~f:(fun segment -> Segment.to_bytes segment) in
  Bytes.Slice.join ~sep:(Bytes.Slice.of_string_slice (String.C.Slice.of_string "/")) bslices

let pp t formatter =
  formatter |> String.pp (to_string_replace t)
