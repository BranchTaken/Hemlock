open Rudiments0

module T = struct
  type t = string

  let blength t =
    Stdlib.(Int64.of_int (String.length t))

  let hash_fold t state =
    state
    |> Hash.State.Gen.init
    |> Hash.State.Gen.fold_u8 Stdlib.(Int64.of_int (String.length t)) ~f:(fun i ->
      Stdlib.(Int64.of_int (Char.code (String.get t (Int64.to_int i))))
    )
    |> Hash.State.Gen.fini
    |> Uns.hash_fold (blength t)

  let cmp t0 t1 =
    let open Cmp in
    let rel = Sint.extend_of_int (compare t0 t1) in
    if Sint.(rel < 0L) then
      Lt
    else if Sint.(rel = 0L) then
      Eq
    else
      Gt

  module CodepointSeq = struct
    module T = struct
      type outer = t
      type t = {
        string: outer;
        bindex: uns;
      }

      let init t =
        {string=t; bindex=0L}

      let length t =
        (blength t.string) - t.bindex

      let next t =
        match length t with
        | 0L -> None
        | _ -> begin
            let b = Byte.of_char Stdlib.(String.get t.string (Int64.to_int t.bindex)) in
            let t' = {t with bindex=(Uns.succ t.bindex)} in
            Some (b, t')
          end
    end
    include T
    include Codepoint.Seq.Make(T)
  end

  let pp ppf t =
    let rec fn seq = begin
      match CodepointSeq.to_codepoint seq with
      | Some (Valid (cp, seq')) -> begin
          Format.fprintf ppf "%s" Codepoint.Utf8.(escape (of_codepoint cp));
          fn seq'
        end
      | Some (Invalid _) -> not_reached ()
      | None -> ()
    end in
    Format.fprintf ppf "\"";
    fn (CodepointSeq.init t);
    Format.fprintf ppf "\""

  (* For internal use only, needed due to shadowing. *)
  let pp_string = pp

  let of_string s =
    s

  let to_string t =
    t
end
include T
include Identifiable.Make(T)

let get bindex t =
  Byte.narrow_of_sint_hlt (sint_of_int Stdlib.(Char.code (String.get t (Int64.to_int bindex))))

module Cursor = struct
  module T = struct
    type container = t
    type elm = codepoint
    type t = {
      string: container;
      bindex: uns;
    }

    let hash_fold t state =
      state
      |> hash_fold t.string
      |> Uns.hash_fold t.bindex

    let cmp t0 t1 =
      (* == is excessively vague in OCaml. *)
      assert ((t0.string == t1.string) || (t0.string = t1.string));
      Uns.cmp t0.bindex t1.bindex

    let pp ppf t =
      Format.fprintf ppf "@[<h>{string=%a,@ bindex=%a}@]"
        pp t.string
        Uns.pp t.bindex
  end
  include T
  include Identifiable.Make(T)

  let hd string =
    {string; bindex=0L}

  let tl string =
    {string; bindex=(blength string)}

  let string t =
    t.string

  let container = string

  let index t =
    t.bindex

  let bindex t =
    t.bindex

  let cindex _ =
    not_reached ()

  let at ~bindex string =
    let tl_index = blength string in
    if Uns.(bindex = tl_index) then
      {string; bindex}
    else begin
      let b:byte = get bindex string in
      if Byte.((bit_and b (kv 0b11_000000L)) <> (kv 0b10_000000L)) then
        {string; bindex}
      else
        halt "Not at code point boundary"
    end

  let near ~bindex string =
    let tl_index = blength string in
    if Uns.(bindex = tl_index) then
      {string; bindex}
    else begin
      let rec fn bindex = begin
        let b = get bindex string in
        match Byte.((bit_and b (kv 0b11_000000L)) <> (kv 0b10_000000L)) with
        | true -> {string; bindex}
        | false -> fn (Uns.pred bindex)
      end in
      fn bindex
    end

  let seek coffset t =
    let rec left coffset t = begin
      match Sint.(coffset = 0L) with
      | true -> t
      | false -> begin
          let t' = near t.string ~bindex:(Uns.pred t.bindex) in
          let coffset' = Sint.(pred coffset) in
          left coffset' t'
        end
    end in
    let rec right coffset t = begin
      match Sint.(coffset = 0L) with
      | true -> t
      | false -> begin
          let b = get t.bindex t.string in
          let nbytes =
            if Byte.(b <= (kv 0b0111_1111L)) then 1L
            else Byte.(bit_clz (bit_not b))
          in
          let t' = {t with bindex=t.bindex + nbytes} in
          let coffset' = Sint.(pred coffset) in
          right coffset' t'
        end
    end in
    if Sint.(coffset < 0L) then
      left Sint.(neg coffset) t
    else
      right coffset t

  let pred t =
    seek (Sint.kv (-1L)) t

  let succ t =
    seek (Sint.kv 1L) t

  let prev t =
    let bindex = Uns.pred t.bindex in
    let b = Byte.extend_to_uns (get bindex t.string) in
    if Uns.(b <= 0b0111_1111L) then (Codepoint.trunc_of_uns b), {t with bindex}
    else begin
      let rec fn string bindex u nbits = begin
        let b = Byte.extend_to_uns (get bindex string) in
        match Uns.((bit_and b 0b11_000000L) <> 0b10_000000L) with
        | true -> begin
            let mask = bit_usr ~shift:(nbits/6L) 0x3fL in
            let bits = bit_and b mask in
            let u' = bit_or u (bit_sl ~shift:nbits bits) in
            (Codepoint.trunc_of_uns u'), {string; bindex}
          end
        | false -> begin
            let bindex' = Uns.pred bindex in
            let bits = bit_and b 0b00_111111L in
            let u' = bit_or (bit_sl ~shift:nbits bits) u in
            let nbits' = nbits + 6L in
            fn string bindex' u' nbits'
          end
      end in
      let bindex' = Uns.pred bindex in
      let u = bit_and b 0b00_111111L in
      let nbits = 6L in
      fn t.string bindex' u nbits
    end

  let next t =
    let b = Byte.extend_to_uns (get t.bindex t.string) in
    let bindex' = Uns.succ t.bindex in
    if Uns.(b <= 0b0111_1111L) then (Codepoint.trunc_of_uns b), {t with bindex=bindex'}
    else begin
      let rec fn u string bindex rem_bytes = begin
        match rem_bytes with
        | 0L -> (Codepoint.trunc_of_uns u), {string; bindex}
        | _ -> begin
            let b = Byte.extend_to_uns (get bindex string) in
            let bits = bit_and b 0b00_111111L in
            let u' = bit_or (bit_sl ~shift:6L u) bits in
            let bindex' = Uns.succ bindex in
            fn u' string bindex' (Uns.pred rem_bytes)
          end
      end in
      let nbytes = Byte.(bit_clz (bit_not (trunc_of_uns b))) in
      let b0_nbits = 7L - nbytes in
      let b0_mask = (bit_sl ~shift:b0_nbits 1L) - 1L in
      let u = bit_and b b0_mask in
      fn u t.string bindex' (Uns.pred nbytes)
    end

  let lget t =
    match prev t with cp, _ -> cp

  let rget t =
    match next t with cp, _ -> cp
end
type cursor = Cursor.t

let clength t =
  let past = Cursor.tl t in
  let rec fn cursor cindex = begin
    match Cursor.(cursor = past) with
    | true -> cindex
    | false -> fn (Cursor.succ cursor) (Uns.succ cindex)
  end in
  fn (Cursor.hd t) 0L

module Cursori = struct
  module T = struct
    type outer = t
    type t = {
      cursor: cursor;
      cindex: uns;
    }

    let hash_fold t state =
      state
      |> Cursor.hash_fold t.cursor
      |> Uns.hash_fold t.cindex

    let cmp t0 t1 =
      Cursor.cmp t0.cursor t1.cursor

    let pp ppf t =
      Format.fprintf ppf "@[<h>{cursor=%a,@ cindex=%a}@]"
        Cursor.pp t.cursor
        Uns.pp t.cindex
  end
  include T
  include Identifiable.Make(T)

  let hd string =
    {cursor=(Cursor.hd string); cindex=0L}

  let tl string =
    {cursor=(Cursor.tl string); cindex=(clength string)}

  let string t =
    Cursor.string t.cursor

  let container = string

  let index t =
    Cursor.index t.cursor

  let bindex t =
    Cursor.bindex t.cursor

  let seek coffset t =
    (* coffset may be negative, but it's okay to convert blindly to uns because 2s complement
     * addition does the right thing. *)
    {cursor=(Cursor.seek coffset t.cursor);
      cindex=Uns.(t.cindex + (bits_of_sint coffset))}

  let pred t =
    {cursor=(Cursor.pred t.cursor); cindex=(Uns.pred t.cindex)}

  let succ t =
    {cursor=(Cursor.succ t.cursor); cindex=(Uns.succ t.cindex)}

  let lget t =
    Cursor.lget t.cursor

  let rget t =
    Cursor.rget t.cursor

  let prev t =
    lget t, pred t

  let next t =
    rget t, succ t

  let cursor t =
    t.cursor

  let cindex t =
    t.cindex

  let at ~cindex s =
    {cursor=(Cursor.seek (Uns.bits_to_sint cindex) (Cursor.hd s)); cindex}
end
type cursori = Cursori.t

module SlicePre = struct
  include Slice.MakeMonoIter(Cursor)
end

module Seq = struct
  type outer = t
  module type S = sig
    type t
    val to_string: t -> outer
  end

  module Codepoint = struct
    module Make (T : SeqIntf.IMonoDef with type elm := codepoint) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | 0L -> ""
        | _ -> begin
            let tmut = ref t in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init (Uns.trunc_to_int len) (fun _ ->
              match !rem_bytes with
              | [] -> begin
                  let cp, t' = T.next !tmut in
                  assert (Uns.((Codepoint.Utf8.length_of_codepoint cp) +
                      (T.length t') = (T.length !tmut)));
                  tmut := t';
                  let b, tl = match Codepoint.to_bytes cp with
                    | b :: tl -> b, tl
                    | [] -> not_reached ()
                  in
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                end
            ) in
            assert (Uns.((List.length !rem_bytes) = 0L));
            s
          end
    end

    module MakeRev (T : SeqIntf.IMonoDef with type elm := codepoint) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | 0L -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the codepoints in order to
             * manually reverse them. *)
            let rec fn t cps = begin
              match T.length t with
              | 0L -> cps
              | _ -> begin
                  let cp, t' = T.next t in
                  assert (Uns.((Codepoint.Utf8.length_of_codepoint cp) + (T.length t') =
                    (T.length t)));
                  let cps' = cp :: cps in
                  fn t' cps'
                end
            end in
            let cps = ref (fn t []) in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init (Uns.trunc_to_int len) (fun _ ->
              match !rem_bytes with
              | [] -> begin
                  let cp, cps' = match !cps with
                    | cp :: cps' -> cp, cps'
                    | [] -> not_reached ()
                  in
                  cps := cps';
                  let b, tl = match Codepoint.to_bytes cp with
                    | b :: tl -> b, tl
                    | [] -> not_reached ()
                  in
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                end
            ) in
            assert (Uns.((List.length !rem_bytes) = 0L));
            s
          end
    end
  end

  module Slice = struct
    module Make (T : SeqIntf.IMonoDef with type elm := SlicePre.t) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | 0L -> ""
        | _ -> begin
            let tmut = ref t in
            let slice_str = ref "" in
            let slice_base = ref 0L in
            let slice_ind = ref 0L in
            let slice_len = ref 0L in
            let s = Stdlib.String.init (Uns.trunc_to_int len) (fun _ ->
              let rec fn () = begin
                match Uns.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, t' = T.next !tmut in
                    let slice_base' = Cursor.bindex (SlicePre.base slice) in
                    let slice_len' = (Cursor.bindex (SlicePre.past slice)) - slice_base' in
                    assert (Uns.(slice_len' + (T.length t') = (T.length !tmut)));
                    tmut := t';

                    slice_str := Cursor.string (SlicePre.base slice);
                    slice_base := slice_base';
                    slice_ind := 0L;
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get (!slice_base + !slice_ind) !slice_str in
                    slice_ind := Uns.succ !slice_ind;
                    Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                  end
              end in
              fn ()
            ) in
            assert Uns.(!slice_ind = !slice_len);
            s
          end
    end

    module MakeRev (T : SeqIntf.IMonoDef with type elm := SlicePre.t) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | 0L -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the strings in order to manually
             * reverse them. *)
            let rec fn t slices = begin
              match T.length t with
              | 0L -> slices
              | _ -> begin
                  let slice, t' = T.next t in
                  let slices' = slice :: slices in
                  fn t' slices'
                end
            end in
            let slices = ref (fn t []) in

            let slice_str = ref "" in
            let slice_base = ref 0L in
            let slice_ind = ref 0L in
            let slice_len = ref 0L in
            let s = Stdlib.String.init (Uns.trunc_to_int len) (fun _ ->
              let rec fn () = begin
                match Uns.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, slices' = match !slices with
                      | slice :: slices' -> slice, slices'
                      | [] -> not_reached ()
                    in
                    let slice_base' = Cursor.bindex (SlicePre.base slice) in
                    let slice_len' = (Cursor.bindex (SlicePre.past slice)) - slice_base' in
                    slices := slices';
                    slice_str := Cursor.string (SlicePre.base slice);
                    slice_base := slice_base';
                    slice_ind := 0L;
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get (!slice_base + !slice_ind) !slice_str in
                    slice_ind := Uns.succ !slice_ind;
                    Stdlib.Char.chr (int_of_sint (Byte.extend_to_sint b))
                  end
              end in
              fn ()
            ) in
            assert Uns.(!slice_ind = !slice_len);
            s
          end
    end
  end

  module String = struct
    module Make (T : SeqIntf.IMonoDef with type elm := string) :
      S with type t := T.t = struct
      module U = struct
        type t = T.t

        let length = T.length

        let next t =
          let str, t' = T.next t in
          let slice = SlicePre.init str in
          slice, t'
      end
      include U
      include Slice.Make(U)
    end

    module MakeRev (T : SeqIntf.IMonoDef with type elm := string) :
      S with type t := T.t = struct
      module U = struct
        type t = T.t

        let length = T.length

        let next t =
          let str, t' = T.next t in
          let slice = SlicePre.init str in
          slice, t'
      end
      include U
      include Slice.MakeRev(U)
    end
  end
end

module Slice = struct
  module T = struct
    type outer = string
    include SlicePre

    let hash_fold t state =
      state
      |> Cursor.hash_fold (base t)
      |> Cursor.hash_fold (past t)

    let cmp t0 t1 =
      match Cursor.cmp (base t0) (base t1) with
      | Lt -> Cmp.Lt
      | Eq -> begin
          (* Consider contained slices to come after their containers in the total order. *)
          match Cursor.cmp (past t0) (past t1) with
          | Lt -> Cmp.Gt
          | Eq -> Cmp.Eq
          | Gt -> Cmp.Lt
        end
      | Gt -> Cmp.Gt

    let pp ppf t =
      Format.fprintf ppf "@[<h>{base=%a,@ past=%a}@]"
        Cursor.pp (base t)
        Cursor.pp (past t)
  end
  include T
  include Identifiable.Make(T)

  let string t =
    SlicePre.container t

  let of_string ?base ?past s =
    SlicePre.init ?base ?past s

  module StringSlice = struct
    module T = struct
      type t = SlicePre.t

      let length t =
        (Cursor.bindex (past t)) - (Cursor.bindex (base t))

      let next t =
        let t' = of_string "" in
        t, t'
    end
    include T
    include Seq.Slice.Make(T)
  end

  let to_string t =
    let s = string t in
    match Cursor.(base t = (hd s)) && Cursor.(past t = (tl s)) with
    | true -> s (* Avoid creating an exact copy. *)
    | false -> StringSlice.to_string t

  let string_blength = blength

  let blength t =
    (Cursor.bindex (past t)) - (Cursor.bindex (base t))

  let string_clength = clength

  let clength t =
    let s = string t in
    match Cursor.((base t) = (hd s)) && Cursor.((past t) = (tl s)) with
    | true -> clength s
    | false -> begin
        match Uns.((clength s) = (string_blength s)) with
        | true -> blength t
        | false -> begin
            let rec fn cursor cindex = begin
              match Cursor.(cursor = (past t)) with
              | true -> cindex
              | false -> fn (Cursor.succ cursor) (Uns.succ cindex)
            end in
            fn (base t) 0L
          end
      end

  let get bindex t =
    match Uns.(bindex > (blength t)) with
    | true -> halt "Out of bounds"
    | false -> begin
        let bind = ((Cursor.bindex (base t)) + bindex) in
        Byte.narrow_of_uns_hlt (Uns.extend_of_int Stdlib.(Char.code (String.unsafe_get (string t)
          (Int64.to_int bind))))
      end

  module StringOfIndexed = struct
    module T = struct
      type t = {
        f: uns -> codepoint;
        blength: uns;
        cindex: uns;
      }

      let init ~f blength cindex =
        {f; blength; cindex}

      let length t =
        t.blength

      let next t =
        let codepoint = t.f t.cindex in
        let cp_nbytes = Codepoint.Utf8.length_of_codepoint codepoint in
        let blength' = t.blength - cp_nbytes in
        let t' = {t with cindex=(Uns.succ t.cindex); blength=blength'} in
        codepoint, t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_crange crange ~f =
    Range.fold crange ~init:0L ~f:(fun nbytes cindex ->
      let codepoint = f cindex in
      let cp_nbytes = Codepoint.Utf8.length_of_codepoint codepoint in
      nbytes + cp_nbytes
    )

  let init ?blength crange ~f =
    let blength = match blength with
      | None -> blength_of_crange crange ~f
      | Some blength -> blength
    in
    of_string StringOfIndexed.(to_string (init ~f blength (Range.base crange)))

  let of_codepoint codepoint =
    init (0L =:< 1L) ~f:(fun _ -> codepoint)

  module StringOfListCommon = struct
    type t = {
      codepoints: codepoint list;
      blength: uns;
    }

    let init codepoints blength =
      {codepoints; blength}

    let length t =
      t.blength

    let next t =
      let codepoint, codepoints = match t.codepoints with
        | cp :: cps -> cp, cps
        | [] -> not_reached ()
      in
      let nbytes = Codepoint.Utf8.length_of_codepoint codepoint in
      let blength = t.blength - nbytes in
      codepoint, {codepoints; blength}
  end

  module StringOfList = struct
    include StringOfListCommon
    include Seq.Codepoint.Make(StringOfListCommon)
  end

  module StringOfListRev = struct
    include StringOfListCommon
    include Seq.Codepoint.MakeRev(StringOfListCommon)
  end

  let blength_of_list codepoints =
    List.fold codepoints ~init:0L ~f:(fun nbytes cp ->
      let cp_nbytes = Codepoint.Utf8.length_of_codepoint cp in
      nbytes + cp_nbytes
    )

  let of_list ?blength codepoints =
    let blength = match blength with
      | None -> blength_of_list codepoints
      | Some blength -> blength
    in
    of_string StringOfList.(to_string (init codepoints blength))

  let of_list_rev ?blength codepoints_rev =
    let blength = match blength with
      | None -> blength_of_list codepoints_rev
      | Some blength -> blength
    in
    of_string StringOfListRev.(to_string (init codepoints_rev blength))

  let of_array ?blength codepoints =
    init ?blength (0L =:< (Array.length codepoints)) ~f:(fun i ->
      Array.get i codepoints
    )

  module U = struct
    type outer = t
    type t = outer
    type elm = codepoint

    module Cursor = struct
      module T = struct
        type t = cursor

        let cmp = Cursor.cmp

        let hd slice =
          (base slice)

        let tl slice =
          (past slice)

        let pred = Cursor.pred

        let succ = Cursor.succ

        let lget = Cursor.lget

        let rget = Cursor.rget

        let prev = Cursor.prev

        let next = Cursor.next
      end
      include T
      include Cmpable.Make(T)
    end

    let cmp_elm = Codepoint.cmp

    let length = clength
  end
  include Container.MakeMono(U)

  module StringMapi = struct
    module T = struct
      type t = {
        f: uns -> codepoint -> codepoint;
        cursor: cursor;
        cindex: uns;
        blength: uns;
      }

      let init slice ~f blength =
        {
          f;
          cursor=base slice;
          cindex=0L;
          blength
        }

      let length t =
        t.blength

      let next t =
        let codepoint = Cursor.rget t.cursor in
        let codepoint' = t.f t.cindex codepoint in
        let utf8_length = Codepoint.Utf8.length_of_codepoint codepoint' in
        let cursor' = Cursor.succ t.cursor in
        let cindex' = Uns.succ t.cindex in
        let blength' = t.blength - utf8_length in
        let t' = {t with cursor=cursor'; cindex=cindex'; blength=blength'} in
        codepoint', t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_map t ~f =
    foldi t ~init:(0L, false) ~f:(fun i (blength, modified) codepoint ->
      let codepoint' = f i codepoint in
      let modified' = modified || Codepoint.(codepoint' <> codepoint) in
      (blength + (Codepoint.Utf8.length_of_codepoint codepoint')), modified'
    )

  let map ~f t =
    let f' _ codepoint = begin
      f codepoint
    end in
    let blength, modified = blength_of_map t ~f:f' in
    match modified with
    | false -> t
    | true -> of_string StringMapi.(to_string (init t blength ~f:f'))

  let mapi ~f t =
    let blength, modified = blength_of_map t ~f in
    match modified with
    | false -> t
    | true -> of_string StringMapi.(to_string (init t blength ~f))

  let tr ~target ~replacement t =
    let f _ codepoint = begin
      if Codepoint.(codepoint = target) then replacement
      else codepoint
    end in
    let blength, modified = blength_of_map t ~f in
    match modified with
    | false -> t
    | true -> of_string StringMapi.(to_string (init t blength ~f))

  let filter ~f t =
    let codepoints, modified = fold_right t ~init:([], false)
      ~f:(fun codepoint (codepoints, modified) ->
        if f codepoint then codepoint :: codepoints, modified
        else codepoints, true
      ) in
    match modified with
    | false -> t
    | true -> of_list codepoints

  module StringConcat = struct
    module T = struct
      type outer = t
      type source =
        | Sep
        | Str
      type t = {
        sep: outer;
        slices: outer list;
        source: source;
        blength: uns;
      }

      let init sep slices blength =
        {sep=sep; slices; source=Str; blength}

      let length t =
        t.blength

      let next t =
        let rec fn t = begin
          match t.source with
          | Sep -> begin
              match blength t.sep with
              | 0L -> fn {t with source=Str}
              | _ -> begin
                  let blength' = t.blength - (blength t.sep) in
                  t.sep, {t with source=Str; blength=blength'}
                end
            end
          | Str -> begin
              match t.slices with
              | [] -> not_reached ()
              | slice :: slices' -> begin
                  let blength' = t.blength - (blength slice) in
                  slice, {t with slices=slices'; source=Sep; blength=blength'}
                end
            end
        end in
        fn t
    end
    include T
    include Seq.Slice.Make(T)
  end

  let concat ?(sep=(of_string "")) (slices:t list) =
    let _, blength = List.fold slices ~init:(0L, 0L)
      ~f:(fun (i, len) slice ->
        let i' = Uns.succ i in
        let sep_len = match i with
          | 0L -> 0L
          | _ -> blength sep
        in
        let len' = sep_len + len + (blength slice) in
        i', len'
      ) in
    match (length sep), (List.length slices) with
    | _, 0L -> of_string ""
    | 0L, 1L -> List.hd slices
    | _ -> of_string (StringConcat.to_string (StringConcat.init sep slices blength))

  let concat_rev ?(sep=(of_string "")) slices_rev =
    let slices, blength = List.fold slices_rev ~init:([], 0L)
      ~f:(fun (slices, len) slice ->
        let slices' = slice :: slices in
        let sep_len = match slices with
          | [] -> 0L
          | _ -> blength sep
        in
        let len' = sep_len + len + (blength slice) in
        slices', len'
      ) in
    match (length sep), (List.length slices) with
    | _, 0L -> of_string ""
    | 0L, 1L -> List.hd slices
    | _ -> of_string (StringConcat.to_string
        (StringConcat.init sep slices blength))

  let concat_map ?sep ~f t =
    (* Iterate in reverse order to generate a list of slices that can then be passed to concat. *)
    let modified, slices = fold_right t ~init:(false, [])
      ~f:(fun cp (modified, slices) ->
        let slice = f cp in
        let modified' = modified
                        || Uns.((blength slice) <> (Codepoint.Utf8.length_of_codepoint cp))
                        || Codepoint.(Cursor.(rget (base slice)) <> cp) in
        let slices' = slice :: slices in
        modified', slices'
      ) in
    match modified, sep with
    | false, None -> t
    | _ -> concat ?sep slices

  let escaped t =
    concat_map t ~f:(fun cp -> of_string Codepoint.Utf8.(escape (of_codepoint cp)))

  module StringRev = struct
    module T = struct
      type outer = t
      type t = {
        slice: outer;
        cursori: cursori;
      }

      let init slice =
        {slice; cursori=Cursori.hd (string slice)}

      let length t =
        (clength t.slice) - (Cursori.cindex t.cursori)

      let next t =
        let codepoint = Cursori.rget t.cursori in
        let t' = {t with cursori=(Cursori.succ t.cursori)} in
        codepoint, t'
    end
    include T
    include Seq.Codepoint.MakeRev(T)
  end

  let rev t =
    of_string StringRev.(to_string (init t))

  let lfind codepoint t =
    let rec fn cursor = begin
      match Cursor.(cursor = past t) with
      | true -> None
      | false -> begin
          let cp = Cursor.rget cursor in
          match Codepoint.(cp = codepoint) with
          | true -> Some cursor
          | false -> fn (Cursor.succ cursor)
        end
    end in
    fn (base t)

  let lfind_hlt codepoint t =
    match lfind codepoint t with
    | None -> halt "Codepoint not found"
    | Some cursor -> cursor

  let contains codepoint t =
    match lfind codepoint t with
    | None -> false
    | Some _ -> true

  let rfind codepoint t =
    let rec fn cursor = begin
      match Cursor.(cursor = base t) with
      | true -> None
      | false -> begin
          let cp = Cursor.lget cursor in
          let cursor' = Cursor.pred cursor in
          match Codepoint.(cp = codepoint) with
          | true -> Some cursor'
          | false -> fn cursor'
        end
    end in
    fn (past t)

  let rfind_hlt codepoint t =
    match rfind codepoint t with
    | None -> halt "Codepoint not found"
    | Some codepoint -> codepoint

  module Pattern = struct
    type outer_string = outer
    type outer = t
    type t = {
      (* Pattern slice. *)
      p: outer_string;
      (* Auxilliary data structure that enables Knuth-Morris-Pratt (KMP) pattern matching. The
       * cursors correspond to the codepoints of p. Each cursor points to the end of the longest
       * pattern prefix that has an equivalent suffix in the substring ending just prior to the
       * corresponding codepoint index. *)
      pi: cursori array;
    }

    let create slice =
      let s = string slice in
      let m = Cursori.tl s in
      let rec compute_pi ~p ~k ~q ~pi = begin
        match Cursori.(q = m) with
        | true -> pi
        | false -> begin
            let k_eq_q ~k ~q =
              Codepoint.((Cursori.rget k) = (Cursori.rget q))
            in
            match ((not (k_eq_q ~k ~q)) &&
                   Uns.is_positive (Cursori.cindex k)) with
            | true ->
              let k' = Array.get (Uns.pred (Cursori.cindex k)) pi in
              compute_pi ~p ~k:k' ~q ~pi
            | false -> begin
                let k' = match (k_eq_q ~k ~q) with
                  | true -> Cursori.(succ k)
                  | false -> k
                in
                let q' = Cursori.succ q in
                Array.set_inplace (Cursori.cindex q) k' pi;
                compute_pi ~p ~k:k' ~q:q' ~pi
              end
          end
      end in
      let pi = match Cursori.cindex m with
        | 0L -> [||]
        | _ -> begin
            let k = Cursori.hd s in
            let q = Cursori.succ k in
            let pi = Array.init (0L =:< (Cursori.cindex m)) ~f:(fun _ -> k) in
            compute_pi ~p:s ~k ~q ~pi
          end
      in
      {p=s; pi}

    let pp ppf t =
      let open Format in
      fprintf ppf "@[<v> p=%a@,pi=[" pp_string t.p;
      Array.iter t.pi ~f:(fun elm ->
        fprintf ppf "%a" Uns.pp (Cursori.cindex elm)
      );
      Format.fprintf ppf "]@]"

    let find_impl ?max_matches ~may_overlap ~in_ t =
      let past = past in_ in
      let m = Cursori.tl t.p in
      let rec fn ~q ~i matches nmatches = begin
        match max_matches, Cursori.(q = m) with
        | Some n, _ when Uns.(nmatches = n) -> List.rev matches
        | _, true -> begin
            let cursor = (Cursor.at (string in_) ~bindex:((Cursor.bindex (base in_)) +
                (Cursor.bindex i) - (Cursori.bindex m))) in
            let matches' = cursor :: matches in
            let nmatches' = Uns.succ nmatches in
            match may_overlap, Uns.((Cursori.bindex m) = 0L), Cursor.(i = past) with
            (* Empty pattern; terminate scanning or advance i to avoid multiple matches at the same
             * position. *)
            | _, true, true -> List.rev matches'
            | _, true, false -> begin
                let q' = Cursori.hd t.p in
                let i' = Cursor.succ i in
                fn ~q:q' ~i:i' matches' nmatches'
              end
            (* Attempt overlapping match. *)
            | true, false, _ -> begin
                let q' = Array.get (Uns.pred (Cursori.cindex q)) t.pi in
                fn ~q:q' ~i matches' nmatches'
              end
            (* Discard lookbehind to avoid overlapping matches. *)
            | false, _, _ -> begin
                let q' = Cursori.hd t.p in
                fn ~q:q' ~i matches' nmatches'
              end
          end
        | _, false -> begin
            match Cursor.(i = past) with
            | true -> List.rev matches
            | false -> begin
                let q_eq_i ~q ~i =
                  Codepoint.((Cursori.rget q) = (Cursor.rget i))
                in
                match (not (q_eq_i ~q ~i)) && (Uns.is_positive (Cursori.cindex q)) with
                | true -> begin
                    let q' = Array.get (Uns.pred (Cursori.cindex q)) t.pi in
                    fn ~q:q' ~i matches nmatches
                  end
                | false -> begin
                    let q' = match (q_eq_i ~q ~i) with
                      | true -> Cursori.succ q
                      | false -> q
                    in
                    let i' = Cursor.succ i in
                    fn ~q:q' ~i:i' matches nmatches
                  end
              end
          end
      end in
      let q = Cursori.hd t.p in
      let i = base in_ in
      fn ~q ~i [] 0L

    let find ~in_ t =
      let cursors = find_impl ~max_matches:1L ~may_overlap:false ~in_ t in
      match cursors with
      | [] -> None
      | cursor :: [] -> Some cursor
      | _ :: _ -> not_reached ()

    let find_hlt ~in_ t =
      match find ~in_ t with
      | None -> halt "No match"
      | Some cursor -> cursor

    let find_all ~may_overlap ~in_ t =
      find_impl ~may_overlap ~in_ t

    module StringPatternReplace = struct
      module T = struct
        type source =
          | In
          | With
        type t = {
          in_: outer;
          pattern: outer_string;
          with_: outer;
          at: cursor list;
          in_cursor: cursor;
          slice: SlicePre.t;
          source: source;
          blength: uns;
        }

        let init ~pattern ~in_ ~with_ ~at =
          let in_cursor, slice, at', source = match at with
            | [] -> base in_, in_, at, In
            | cursor :: at' when Cursor.(cursor = base in_) ->
              let in_cursor = (Cursor.at cursor.string ~bindex:( cursor.bindex + (string_blength
                  pattern))) in
              let slice = with_ in
              in_cursor, slice, at', With
            | cursor :: _ -> begin
                let slice = of_string ~base:(base in_) ~past:cursor (Cursor.container cursor) in
                past in_, slice, at, In
              end
          in
          let ncursors = List.length at in
          let blength = ((blength in_) + (ncursors * ((blength with_) - (string_blength
              pattern)))) in
          {in_; pattern; with_; at=at'; in_cursor; slice; source; blength}

        let length t =
          t.blength

        let next t =
          let blength' = (t.blength - Cursor.((bindex (past t.slice)) - bindex (base t.slice))) in
          let t' = match t.source with
            | In -> begin
                let in_cursor', slice', at' = match t.at with
                  | [] -> begin
                      let slice' = of_string ~base:(past t.in_) ~past:(past t.in_) (container t.in_)
                      in
                      past t.in_, slice', []
                    end
                  | cursor :: at' -> begin
                      let in_cursor' = Cursor.seek (Uns.bits_to_sint (string_clength t.pattern))
                        cursor in
                      let slice' = t.with_ in
                      in_cursor', slice', at'
                    end
                in
                {t with at=at'; in_cursor=in_cursor'; slice=slice'; source=With; blength=blength'}
              end
            | With -> begin
                let slice' = match t.at with
                  | [] -> of_string ~base:t.in_cursor ~past:(past t.in_) (container t.in_)
                  | cursor :: _ -> of_string ~base:t.in_cursor ~past:cursor (container t.in_)
                in
                {t with slice=slice'; source=In; blength=blength'}
              end
          in t.slice, t'
      end
      include T
      include Seq.Slice.Make(T)
    end

    let replace_first ~in_ ~with_ t =
      match find t ~in_ with
      | None -> in_
      | Some cursor ->
        of_string (StringPatternReplace.(to_string (init ~pattern:t.p ~in_ ~with_ ~at:[cursor])))

    let replace_all ~in_ ~with_ t =
      match find_all t ~may_overlap:false ~in_ with
      | [] -> in_
      | cursors ->
        of_string (StringPatternReplace.(to_string (init ~pattern:t.p ~in_ ~with_ ~at:cursors)))
  end

  let prefix_tl ~prefix t =
    let rec fn t_cursor prefix_cursor = begin
      let end_of_t = Cursor.(t_cursor = past t) in
      let end_of_prefix = Cursor.(prefix_cursor = past prefix) in
      match end_of_t, end_of_prefix with
      | true, false -> None
      | _, true -> Some t_cursor
      | false, false -> begin
          match Codepoint.((Cursor.rget t_cursor) = (Cursor.rget prefix_cursor)) with
          | false -> None
          | true -> fn (Cursor.succ t_cursor) (Cursor.succ prefix_cursor)
        end
    end in
    fn (base t) (base prefix)

  let is_prefix ~prefix t =
    match prefix_tl ~prefix t with
    | None -> false
    | Some _ -> true

  let suffix_hd ~suffix t =
    let rec fn t_cursor suffix_cursor = begin
      let beg_of_t = Cursor.(t_cursor = base t) in
      let beg_of_suffix = Cursor.(suffix_cursor = base suffix) in
      match beg_of_t, beg_of_suffix with
      | true, false -> None
      | _, true -> Some t_cursor
      | false, false -> begin
          match Codepoint.((Cursor.lget t_cursor) = (Cursor.lget suffix_cursor)) with
          | false -> None
          | true -> fn (Cursor.pred t_cursor) (Cursor.pred suffix_cursor)
        end
    end in
    fn (past t) (past suffix)

  let is_suffix ~suffix t =
    match suffix_hd ~suffix t with
    | None -> false
    | Some _ -> true

  let prefix n t =
    let base = base t in
    let past = match Uns.((clength t) < n) with
      | true -> past t
      | false -> Cursor.(seek (Uns.bits_to_sint n) base)
    in
    of_string ~base ~past (container t)

  let suffix n t =
    let past = past t in
    let base = match Uns.((clength t) < n) with
      | true -> base t
      | false -> Cursor.(seek Sint.(neg (Uns.bits_to_sint n)) past)
    in
    of_string ~base ~past (container t)

  let chop_prefix ~prefix t =
    match prefix_tl ~prefix t with
    | None -> None
    | Some base -> Some (of_string ~base ~past:(past t) (container t))

  let chop_prefix_hlt ~prefix t =
    match chop_prefix ~prefix t with
    | None -> halt "Not a prefix"
    | Some slice -> slice

  let chop_suffix ~suffix t =
    match suffix_hd ~suffix t with
    | None -> None
    | Some past -> Some (of_string ~base:(base t) ~past (container t))

  let chop_suffix_hlt ~suffix t =
    match chop_suffix ~suffix t with
    | None -> halt "Not a suffix"
    | Some s -> s

  let drop_whitespace codepoint =
    match codepoint with
    | cp when Codepoint.(cp = ht) -> true
    | cp when Codepoint.(cp = nl) -> true
    | cp when Codepoint.(cp = cr) -> true
    | cp when Codepoint.(cp = (of_char ' ')) -> true
    | _ -> false

  let strip_base drop t =
    let rec fn cursor = begin
      match drop (Cursor.rget cursor) with
      | false -> cursor
      | true -> fn (Cursor.succ cursor)
    end in
    fn (base t)

  let strip_past drop t =
    let rec fn cursor = begin
      match drop (Cursor.lget cursor) with
      | false -> cursor
      | true -> fn (Cursor.pred cursor)
    end in
    fn (past t)

  let lstrip ?drop t =
    let drop = match drop with
      | None -> drop_whitespace
      | Some drop -> drop
    in
    let base = strip_base drop t in
    of_string ~base ~past:(past t) (container t)

  let rstrip ?drop t =
    let drop = match drop with
      | None -> drop_whitespace
      | Some drop -> drop
    in
    let past = strip_past drop t in
    of_string ~base:(base t) ~past (container t)

  let strip ?drop t =
    let drop = match drop with
      | None -> drop_whitespace
      | Some drop -> drop
    in
    let base = strip_base drop t in
    let past = strip_past drop t in
    of_string ~base ~past (container t)

  let split_fold_until ~init ~on ~f t =
    let rec fn base past accum = begin
      match Cursor.(past = SlicePre.past t) with
      | true -> begin
          let slice = of_string ~base ~past (container t) in
          let accum', _ = f accum slice in
          accum'
        end
      | false -> begin
          match on (Cursor.rget past) with
          | true -> begin
              let base' = Cursor.succ past in
              let past' = base' in
              let slice = of_string ~base ~past (container t) in
              let accum', until = f accum slice in
              match until with
              | true -> accum'
              | false -> fn base' past' accum'
            end
          | false -> fn base (Cursor.succ past) accum
        end
    end in
    fn (base t) (base t) init

  let split_fold ~init ~on ~f t =
    split_fold_until ~init ~on ~f:(fun accum slice -> (f accum slice), false) t

  let split_fold_right_until ~init ~on ~f t =
    let rec fn base past accum = begin
      match Cursor.(base = SlicePre.base t) with
      | true -> begin
          let slice = of_string ~base ~past (container t) in
          let accum', _ = f slice accum in
          accum'
        end
      | false -> begin
          match on (Cursor.lget base) with
          | true -> begin
              let base' = Cursor.pred base in
              let past' = base' in
              let slice = of_string ~base ~past (container t) in
              let accum', until = f slice accum in
              match until with
              | true -> accum'
              | false -> fn base' past' accum'
            end
          | false -> fn (Cursor.pred base) past accum
        end
    end in
    fn (past t) (past t) init

  let split_fold_right ~init ~on ~f t =
    split_fold_right_until ~init ~on ~f:(fun slice accum -> (f slice accum), false) t

  let lines_fold ~init ~f t =
    let rec fn base past cr_seen accum = begin
      match Cursor.(past = SlicePre.past t) with
      | true -> begin
          let slice = of_string ~base ~past (container t) in
          f accum slice
        end
      | false -> begin
          match Cursor.rget past with
          | cp when Codepoint.(cp = nl) -> begin
              let base' = Cursor.succ past in
              let past' = base' in
              let slice = match cr_seen with
                | false -> of_string ~base ~past (container t)
                | true -> of_string ~base ~past:(Cursor.pred past) (container t)
              in
              let accum' = f accum slice in
              fn base' past' false accum'
            end
          | cp when Codepoint.(cp = cr) -> fn base (Cursor.succ past) true accum
          | _ ->  fn base (Cursor.succ past) false accum
        end
    end in
    fn (base t) (base t) false init

  let lines_fold_right ~init ~f t =
    let rec fn base past nl_seen accum = begin
      match Cursor.(base = SlicePre.base t) with
      | true -> begin
          match nl_seen with
          | false -> begin
              let slice = of_string ~base ~past (container t) in
              f slice accum
            end
          | true -> begin
              let slice = of_string ~base:(Cursor.succ base) ~past (container t) in
              let accum' = f slice accum in
              let empty_slice = of_string ~base ~past:base (container t) in
              f empty_slice accum'
            end
        end
      | false -> begin
          match nl_seen with
          | false -> begin
              let nl_seen' = Codepoint.((Cursor.lget base) = nl) in
              fn (Cursor.pred base) past nl_seen' accum
            end
          | true -> begin
              let base', past', nl_seen' = match Cursor.lget base with
                | cp when Codepoint.(cp = nl) -> (Cursor.pred base), base, true
                | cp when Codepoint.(cp = cr) -> (Cursor.pred base), (Cursor.pred base), false
                | _ -> base, base, false
              in
              let slice = of_string ~base:(Cursor.succ base) ~past (container t) in
              let accum' = f slice accum in
              fn base' past' nl_seen' accum'
            end
        end
    end in
    fn (past t) (past t) false init

  let lsplit2 ~on t =
    split_fold_until ~init:None ~on:(fun codepoint ->
      Codepoint.(codepoint = on)
    ) ~f:(fun _ slice ->
      let base = Cursor.succ (past slice) in
      let past = past t in
      let slice2 = of_string ~base ~past (container t) in
      (Some (slice, slice2)), true
    ) t

  let lsplit2_hlt ~on t =
    match lsplit2 ~on t with
    | None -> halt "No split performed"
    | Some slice -> slice

  let rsplit2 ~on t =
    split_fold_right_until ~init:None ~on:(fun codepoint ->
      Codepoint.(codepoint = on)
    ) ~f:(fun slice _ ->
      let base, past = (base t), (Cursor.pred (base slice)) in
      let slice0 = of_string ~base ~past (container t) in
      (Some (slice0, slice)), true
    ) t

  let rsplit2_hlt ~on t =
    match rsplit2 ~on t with
    | None -> halt "No split performed"
    | Some slice -> slice

  module O = struct
    module T = struct
      type nonrec t = t

      let cmp = cmp
    end
    include T
    include Cmpable.Make(T)
  end
end

let slice_pattern_pp = Slice.Pattern.pp

let init ?blength crange ~f =
  Slice.to_string (Slice.init ?blength crange ~f)

let of_codepoint codepoint =
  Slice.(to_string (of_codepoint codepoint))

let of_list ?blength codepoints =
  Slice.to_string (Slice.of_list ?blength codepoints)

let of_list_rev ?blength codepoints_rev =
  Slice.to_string (Slice.of_list_rev ?blength codepoints_rev)

let of_array ?blength codepoints =
  Slice.to_string (Slice.of_array ?blength codepoints)

module U = struct
  type outer = t
  type t = outer
  type elm = codepoint

  module Cursor = struct
    include Cursor
  end

  let cmp_elm = Codepoint.cmp

  let length = clength
end
include Container.MakeMono(U)

let map ~f t =
  Slice.(to_string (map ~f (of_string t)))

let mapi ~f t =
  Slice.(to_string (mapi ~f (of_string t)))

let tr ~target ~replacement t =
  Slice.(to_string (tr ~target ~replacement (of_string t)))

let filter ~f t =
  Slice.(to_string (filter ~f (of_string t)))

let concat ?(sep="") strings =
  let slices_rev = List.fold strings ~init:[]
    ~f:(fun accum s -> (Slice.of_string s) :: accum) in
  Slice.(to_string (concat_rev ~sep:(of_string sep) slices_rev))

let concat_rev ?(sep="") strings_rev =
  let slices = List.fold strings_rev ~init:[]
    ~f:(fun accum s -> (Slice.of_string s) :: accum) in
  Slice.(to_string (concat ~sep:(of_string sep) slices))

let concat_map ?sep ~f t =
  let f = (fun cp -> Slice.of_string (f cp)) in
  Slice.to_string (match sep with
    | None -> Slice.(concat_map ~f (of_string t))
    | Some sep -> Slice.(concat_map ~sep:(of_string sep) ~f (of_string t))
  )

let escaped t =
  Slice.(to_string (escaped (of_string t)))

let rev t =
  Slice.(to_string (rev (of_string t)))

let ( ^ ) t0 t1 =
  concat [t0; t1]

let lfind ?base ?past codepoint t =
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = match past with
    | None -> Cursor.tl t
    | Some cursor -> cursor
  in
  Slice.lfind codepoint (Slice.of_string ~base ~past t)

let lfind_hlt ?base ?past codepoint t =
  match lfind ?base ?past codepoint t with
  | None -> halt "Codepoint not found"
  | Some cursor -> cursor

let contains ?base ?past codepoint t =
  match lfind ?base ?past codepoint t with
  | None -> false
  | Some _ -> true

let rfind ?base ?past codepoint t =
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = match past with
    | None -> Cursor.tl t
    | Some cursor -> cursor
  in
  Slice.rfind codepoint (Slice.of_string ~base ~past t)

let rfind_hlt ?base ?past codepoint t =
  match rfind ?base ?past codepoint t with
  | None -> halt "Codepoint not found"
  | Some codepoint -> codepoint

let substr_find ?base ~pattern t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_string ~base ~past t in
  Slice.Pattern.find ~in_ p

let substr_find_hlt ?base ~pattern t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_string ~base ~past t in
  Slice.Pattern.find_hlt ~in_ p

let substr_find_all ~may_overlap ~pattern t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  Slice.Pattern.find_all ~may_overlap ~in_:(Slice.of_string t) p

let substr_replace_first ?base ~pattern ~with_ t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_string ~base ~past t in
  let with_ = Slice.of_string with_ in
  Slice.(to_string (Pattern.replace_first ~in_ ~with_ p))

let substr_replace_all ~pattern ~with_ t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let in_ = Slice.of_string t in
  let with_ = Slice.of_string with_ in
  Slice.(to_string (Pattern.replace_all ~in_ ~with_ p))

let is_prefix ~prefix t =
  Slice.is_prefix ~prefix:(Slice.of_string prefix) (Slice.of_string t)

let is_suffix ~suffix t =
  Slice.is_suffix ~suffix:(Slice.of_string suffix) (Slice.of_string t)

let pare ~base ~past t =
  match Cursor.(base = (hd t)) && Cursor.(past = (tl t)) with
  | true -> Cursor.container base
  | false -> Slice.to_string (Slice.of_string ~base ~past t)

let prefix n t =
  Slice.(to_string (prefix n (of_string t)))

let suffix n t =
  Slice.(to_string (suffix n (of_string t)))

let chop_prefix ~prefix t =
  let slice_opt = Slice.chop_prefix ~prefix:(Slice.of_string prefix) (Slice.of_string t) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_prefix_hlt ~prefix t =
  Slice.to_string (Slice.chop_prefix_hlt ~prefix:(Slice.of_string prefix) (Slice.of_string t))

let chop_suffix ~suffix t =
  let slice_opt = Slice.chop_suffix ~suffix:(Slice.of_string suffix) (Slice.of_string t) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_suffix_hlt ~suffix t =
  Slice.to_string (Slice.chop_suffix_hlt ~suffix:(Slice.of_string suffix) (Slice.of_string t))

let lstrip ?drop t =
  Slice.(to_string (lstrip ?drop (of_string t)))

let rstrip ?drop t =
  Slice.(to_string (rstrip ?drop (of_string t)))

let strip ?drop t =
  Slice.(to_string (strip ?drop (of_string t)))

let split ~f t =
  Slice.split_fold_right ~init:[] ~on:f ~f:(fun slice strings -> (Slice.to_string slice) :: strings)
    (Slice.of_string t)

let split_rev ~f t =
  Slice.split_fold ~init:[] ~on:f ~f:(fun strings slice -> (Slice.to_string slice) :: strings)
    (Slice.of_string t)

let split_lines t =
  Slice.lines_fold_right (Slice.of_string t) ~init:[] ~f:(fun slice lines ->
    (Slice.to_string slice) :: lines
  )

let split_lines_rev t =
  Slice.lines_fold (Slice.of_string t) ~init:[] ~f:(fun lines slice ->
    (Slice.to_string slice) :: lines
  )

let lsplit2 ~on t =
  match Slice.lsplit2 ~on (Slice.of_string t) with
  | None -> None
  | Some (slice, slice2) -> Some ((Slice.to_string slice), (Slice.to_string slice2))

let lsplit2_hlt ~on t =
  let slice, slice2 = Slice.lsplit2_hlt ~on (Slice.of_string t) in
  (Slice.to_string slice), (Slice.to_string slice2)

let rsplit2 ~on t =
  match Slice.rsplit2 ~on (Slice.of_string t) with
  | None -> None
  | Some (slice, slice2) -> Some ((Slice.to_string slice), (Slice.to_string slice2))

let rsplit2_hlt ~on t =
  let slice, slice2 = Slice.rsplit2_hlt ~on (Slice.of_string t) in
  (Slice.to_string slice), (Slice.to_string slice2)

module O = struct
  module T = struct
    type nonrec t = t

    let cmp = cmp
  end
  include T
  include Cmpable.Make(T)
end
