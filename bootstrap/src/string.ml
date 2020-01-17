open Rudiments

module T = struct
  type t = string

  let hash_fold = Hash.hash_fold

  let cmp t0 t1 =
    let rel = compare t0 t1 in
    if Int.(rel < 0) then
      Cmp.Lt
    else if Int.(rel = 0) then
      Cmp.Eq
    else
      Cmp.Gt

  let sexp_of_t t =
    Sexplib.Std.sexp_of_string t

  let t_of_sexp sexp =
    Sexplib.Std.string_of_sexp sexp

  let of_string s =
    s

  let to_string t =
    t
end
include T
include Identifiable.Make(T)

let blength t =
  Uint.of_int (Stdlib.String.length t)

let is_empty t =
  Uint.((blength t) = (kv 0))

let get t bindex =
  Byte.of_int_hlt (Stdlib.Char.code (Stdlib.String.get t (Uint.to_int bindex)))

module Cursor = struct
  module T = struct
    type outer = t
    type t = {
      string: outer;
      bindex: uint;
    }

    let cmp t0 t1 =
      (* == is excessively vague in OCaml. *)
      assert ((t0.string == t1.string) || (t0.string = t1.string));
      Uint.cmp t0.bindex t1.bindex
  end
  include T
  include Cmpable.Make(T)

  let hd string =
    {string; bindex=(Uint.kv 0)}

  let tl string =
    {string; bindex=(blength string)}

  let string t =
    t.string

  let container = string

  let index _ =
    not_reached ()

  let bindex t =
    t.bindex

  let cindex _ =
    not_reached ()

  let at string ~bindex =
    let tl_index = blength string in
    if Uint.(bindex = tl_index) then
      {string; bindex}
    else begin
      let b:byte = get string bindex in
      if Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) then
        {string; bindex}
      else
        halt "Not at code point boundary"
    end

  let near string ~bindex =
    let tl_index = blength string in
    if Uint.(bindex = tl_index) then
      {string; bindex}
    else begin
      let rec fn bindex = begin
        let b = get string bindex in
        match Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) with
        | true -> {string; bindex}
        | false -> fn (Uint.pred bindex)
      end in
      fn bindex
    end

  let seek t coffset =
    let rec left t coffset = begin
      match coffset with
      | 0 -> t
      | _ -> begin
          let t' = near t.string ~bindex:(Uint.pred t.bindex) in
          let coffset' = Int.(pred coffset) in
          left t' coffset'
        end
    end in
    let rec right t coffset = begin
      match coffset with
      | 0 -> t
      | _ -> begin
          let b = get t.string t.bindex in
          let nbytes =
            if Byte.(b <= (kv 0b0111_1111)) then Uint.kv 1
            else Byte.(bit_clz (bit_not b))
          in
          let t' = {t with bindex=Uint.(t.bindex + nbytes)} in
          let coffset' = Int.(pred coffset) in
          right t' coffset'
        end
    end in
    if Int.(coffset < 0) then
      left t Int.(neg coffset)
    else
      right t coffset

  let succ t =
    seek t 1

  let pred t =
    seek t (-1)

  let lget t =
    let bindex = (Uint.pred t.bindex) in
    let b = get t.string bindex in
    if Byte.(b <= (kv 0b0111_1111)) then Byte.to_codepoint b
    else begin
      let rec fn bindex cp nbits = begin
        let b = get t.string bindex in
        match Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) with
        | true -> begin
            let mask = Byte.(bit_usr (kv 0x3f) Uint.(nbits / (kv 6))) in
            let cp_bits = Byte.(to_codepoint (bit_and b mask)) in
            Codepoint.(bit_or cp (bit_sl cp_bits nbits))
          end
        | false -> begin
            let bindex' = (Uint.pred bindex) in
            let mask = Byte.(kv 0b00_111111) in
            let cp_bits = Byte.(to_codepoint (bit_and b mask)) in
            let cp' = Codepoint.(bit_or cp (bit_sl cp_bits nbits)) in
            let nbits' = Uint.(nbits + (kv 6)) in
            fn bindex' cp' nbits'
          end
      end in
      let bindex' = (Uint.pred bindex) in
      let mask = Byte.(kv 0b00_111111) in
      let cp = Byte.(to_codepoint (bit_and b mask)) in
      let nbits = (Uint.kv 6) in
      fn bindex' cp nbits
    end

  let rget t =
    let b = get t.string t.bindex in
    if Byte.(b <= (kv 0b0111_1111)) then Byte.to_codepoint b
    else begin
      let rec fn cp bindex rem_bytes = begin
        match rem_bytes with
        | rem_bytes when Uint.(rem_bytes = (kv 0)) -> cp
        | _ -> begin
            let b = get t.string bindex in
            let mask = Byte.(kv 0b00_111111) in
            let cp_bits = Byte.(to_codepoint (bit_and b mask)) in
            let cp' = Codepoint.(bit_or (bit_sl cp (Uint.kv 6)) cp_bits) in
            fn cp' (Uint.succ bindex) Uint.(pred rem_bytes)
          end
      end in
      let nbytes = Byte.(bit_clz (bit_not b)) in
      let b0_nbits = Byte.(to_uint ((kv 7) - (of_uint nbytes))) in
      let b0_mask = Byte.((bit_sl one b0_nbits) - one) in
      let cp = Byte.(to_codepoint (bit_and b b0_mask)) in
      fn cp (Uint.succ t.bindex) Uint.(pred nbytes)
    end
end
type cursor = Cursor.t

let clength t =
  let past = Cursor.tl t in
  let rec fn cursor cindex = begin
    match Cursor.(cursor = past) with
    | true -> cindex
    | false -> fn (Cursor.succ cursor) (Uint.succ cindex)
  end in
  fn (Cursor.hd t) (Uint.kv 0)

let length = clength

module Cursori = struct
  module T = struct
    type outer = t
    type t = {
      cursor: cursor;
      cindex: uint;
    }

    let cmp t0 t1 =
      Cursor.cmp t0.cursor t1.cursor
  end
  include T
  include Cmpable.Make(T)

  let hd string =
    {cursor=(Cursor.hd string); cindex=(Uint.kv 0)}

  let tl string =
    {cursor=(Cursor.tl string); cindex=(clength string)}

  let string t =
    Cursor.string t.cursor

  let container = string

  let index _ =
    not_reached ()

  let bindex t =
    Cursor.bindex t.cursor

  let seek t coffset =
    (* coffset may be negative, but it's okay to convert blindly to uint because
     * 2s complement addition does the right thing. *)
    {cursor=(Cursor.seek t.cursor coffset);
     cindex=Uint.(t.cindex + (of_int coffset))}

  let succ t =
    {cursor=(Cursor.succ t.cursor); cindex=(Uint.succ t.cindex)}

  let pred t =
    {cursor=(Cursor.pred t.cursor); cindex=(Uint.pred t.cindex)}

  let lget t =
    Cursor.lget t.cursor

  let rget t =
    Cursor.rget t.cursor

  let cursor t =
    t.cursor

  let cindex t =
    t.cindex

  let at s ~cindex =
    {cursor=(Cursor.seek (Cursor.hd s) (Uint.to_int cindex));
     cindex}
end
type cursori = Cursori.t

type slice = {
  base: cursor;
  past: cursor;
}

let slice_of_cursors ~base ~past =
  assert ((Cursor.string base) = (Cursor.string past));
  assert Uint.((Cursor.bindex base) <= (Cursor.bindex past));
  {base; past}

let slice_of_string t =
  slice_of_cursors ~base:(Cursor.hd t) ~past:(Cursor.tl t)

module Seq = struct
  type outer = t
  module type S = sig
    type t
    val to_string: t -> outer
  end

  module Codepoint = struct
    module Make (T : Seq_intf.I_mono_def with type elm := codepoint) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | len when Uint.(len = (kv 0)) -> ""
        | _ -> begin
            let tmut = ref t in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init (Uint.to_int len) (fun _ ->
              match !rem_bytes with
              | [] -> begin
                  let cp, t' = T.next !tmut in
                  assert (Uint.(Utf8.(length (of_codepoint cp)) + (T.length t')
                      = (T.length !tmut)));
                  tmut := t';
                  let b, tl = match Utf8.(to_bytes (of_codepoint cp)) with
                    | b :: tl -> b, tl
                    | [] -> not_reached ()
                  in
                  rem_bytes := tl;
                  Stdlib.Char.chr (Byte.to_int b)
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (Byte.to_int b)
                end
            ) in
            assert (Uint.((List.length !rem_bytes) = (kv 0)));
            s
          end
    end

    module Make_rev (T : Seq_intf.I_mono_def with type elm := codepoint) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | len when Uint.(len = (kv 0)) -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the
             * codepoints in order to manually reverse them. *)
            let rec fn t cps = begin
              match T.length t with
              | len when Uint.(len = (kv 0)) -> cps
              | _ -> begin
                  let cp, t' = T.next t in
                  assert (Uint.(Utf8.(length (of_codepoint cp)) + (T.length t')
                      = (T.length t)));
                  let cps' = cp :: cps in
                  fn t' cps'
                end
            end in
            let cps = ref (fn t []) in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init (Uint.to_int len) (fun _ ->
              match !rem_bytes with
              | [] -> begin
                  let cp, cps' = match !cps with
                    | cp :: cps' -> cp, cps'
                    | [] -> not_reached ()
                  in
                  cps := cps';
                  let b, tl = match Utf8.(to_bytes (of_codepoint cp)) with
                    | b :: tl -> b, tl
                    | [] -> not_reached ()
                  in
                  rem_bytes := tl;
                  Stdlib.Char.chr (Byte.to_int b)
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (Byte.to_int b)
                end
            ) in
            assert (Uint.((List.length !rem_bytes) = (kv 0)));
            s
          end
    end
  end

  module Slice = struct
    module Make (T : Seq_intf.I_mono_def with type elm := slice) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | len when Uint.(len = (kv 0)) -> ""
        | _ -> begin
            let tmut = ref t in
            let slice_str = ref "" in
            let slice_base = ref (Uint.kv 0) in
            let slice_ind = ref (Uint.kv 0) in
            let slice_len = ref (Uint.kv 0) in
            let s = Stdlib.String.init (Uint.to_int len) (fun _ ->
              let rec fn () = begin
                match Uint.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, t' = T.next !tmut in
                    let slice_base' = Cursor.bindex slice.base in
                    let slice_len' =
                      Uint.((Cursor.bindex slice.past) - slice_base') in
                    assert (Uint.(slice_len' + (T.length t') =
                        (T.length !tmut)));
                    tmut := t';

                    slice_str := Cursor.string slice.base;
                    slice_base := slice_base';
                    slice_ind := (Uint.kv 0);
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get !slice_str Uint.(!slice_base + !slice_ind) in
                    slice_ind := Uint.succ !slice_ind;
                    Stdlib.Char.chr (Byte.to_int b)
                  end
              end in
              fn ()
            ) in
            assert Uint.(!slice_ind = !slice_len);
            s
          end
    end

    module Make_rev (T : Seq_intf.I_mono_def with type elm := slice) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | len when Uint.(len = (kv 0)) -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the strings
             * in order to manually reverse them. *)
            let rec fn t slices = begin
              match T.length t with
              | len when Uint.(len = (kv 0)) -> slices
              | _ -> begin
                  let slice, t' = T.next t in
                  let slices' = slice :: slices in
                  fn t' slices'
                end
            end in
            let slices = ref (fn t []) in

            let slice_str = ref "" in
            let slice_base = ref (Uint.kv 0) in
            let slice_ind = ref (Uint.kv 0) in
            let slice_len = ref (Uint.kv 0) in
            let s = Stdlib.String.init (Uint.to_int len) (fun _ ->
              let rec fn () = begin
                match Uint.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, slices' = match !slices with
                      | slice :: slices' -> slice, slices'
                      | [] -> not_reached ()
                    in
                    let slice_base' = Cursor.bindex slice.base in
                    let slice_len' =
                      Uint.((Cursor.bindex slice.past) - slice_base') in
                    slices := slices';
                    slice_str := Cursor.string slice.base;
                    slice_base := slice_base';
                    slice_ind := (Uint.kv 0);
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get !slice_str Uint.(!slice_base + !slice_ind) in
                    slice_ind := Uint.succ !slice_ind;
                    Stdlib.Char.chr (Byte.to_int b)
                  end
              end in
              fn ()
            ) in
            assert Uint.(!slice_ind = !slice_len);
            s
          end
    end
  end

  module String = struct
    module Make (T : Seq_intf.I_mono_def with type elm := string) :
      S with type t := T.t = struct
      module U = struct
        type t = T.t

        let length = T.length

        let next t =
          let str, t' = T.next t in
          let slice = slice_of_string str in
          slice, t'
      end
      include U
      include Slice.Make(U)
    end

    module Make_rev (T : Seq_intf.I_mono_def with type elm := string) :
      S with type t := T.t = struct
      module U = struct
        type t = T.t

        let length = T.length

        let next t =
          let str, t' = T.next t in
          let slice = slice_of_string str in
          slice, t'
      end
      include U
      include Slice.Make_rev(U)
    end
  end
end

module Slice = struct
  module T = struct
    type outer = t
    type t = slice

    let cmp t0 t1 =
      match Cursor.cmp t0.base t1.base with
      | Lt -> Cmp.Lt
      | Eq -> begin
          (* Consider contained slices to come after their containers in the
           * total order. *)
          match Cursor.cmp t0.past t1.past with
          | Lt -> Cmp.Gt
          | Eq -> Cmp.Eq
          | Gt -> Cmp.Lt
        end
      | Gt -> Cmp.Gt
  end
  include T
  include Cmpable.Make(T)

  let of_cursors ~base ~past =
    slice_of_cursors ~base ~past

  let to_cursors t =
    t.base, t.past

  let string t =
    Cursor.string t.base

  let base t =
    t.base

  let past t =
    t.past

  let of_string s =
    slice_of_string s

  module String_slice = struct
    module T = struct
      type t = slice

      let length t =
        Uint.((Cursor.bindex t.past) - (Cursor.bindex t.base))

      let next t =
        let t' = of_string "" in
        t, t'
    end
    include T
    include Seq.Slice.Make(T)
  end

  let to_string t =
    let s = string t in
    match Cursor.(t.base = (hd s)) && Cursor.(t.past = (tl s)) with
    | true -> s (* Avoid creating an exact copy. *)
    | false -> String_slice.to_string t

  let base_seek t coffset =
    let base' = Cursor.seek t.base coffset in
    {t with base=base'}

  let base_succ t =
    let base' = Cursor.succ t.base in
    {t with base=base'}

  let base_pred t =
    let base' = Cursor.pred t.base in
    {t with base=base'}

  let past_seek t coffset =
    let past' = Cursor.seek t.past coffset in
    {t with past=past'}

  let past_succ t =
    let past' = Cursor.succ t.past in
    {t with past=past'}

  let past_pred t =
    let past' = Cursor.pred t.past in
    {t with past=past'}

  let string_blength = blength

  let blength t =
    Uint.((Cursor.bindex t.past) - (Cursor.bindex t.base))

  let is_empty t =
    Uint.((blength t) = (kv 0))

  let string_clength = clength

  let clength t =
    let s = string t in
    match Cursor.(t.base = (hd s)) && Cursor.(t.past = (tl s)) with
    | true -> clength s
    | false -> begin
        match Uint.((clength s) = (string_blength s)) with
        | true -> blength t
        | false -> begin
            let rec fn cursor cindex = begin
              match Cursor.(cursor = t.past) with
              | true -> cindex
              | false -> fn (Cursor.succ cursor) (Uint.succ cindex)
            end in
            fn t.base (Uint.kv 0)
          end
      end

  let length = clength

  let get t bindex =
    if Uint.(bindex >= (blength t)) then halt "Out of bounds"
    else
      Byte.of_int_hlt (Stdlib.Char.code (Stdlib.String.unsafe_get (string t)
            Uint.(to_int ((Cursor.bindex t.base) + bindex))))

  module String_of_indexed = struct
    module T = struct
      type t = {
        f: uint -> codepoint;
        blength: uint;
        cindex: uint;
      }

      let init ~f blength =
        {f; blength; cindex=(Uint.kv 0)}

      let length t =
        t.blength

      let next t =
        let codepoint = t.f t.cindex in
        let cp_nbytes = Utf8.(length (of_codepoint codepoint)) in
        let blength' = Uint.(t.blength - cp_nbytes) in
        let t' = {t with cindex=(Uint.succ t.cindex);
                         blength=blength'} in
        codepoint, t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_seq clength ~seq ~f =
    let rec fn ~seq cindex nbytes = begin
      match Uint.(cindex = clength) with
      | true -> nbytes
      | false -> begin
          let codepoint, seq' = f seq in
          let cp_nbytes = Utf8.(length (of_codepoint codepoint)) in
          let nbytes' = Uint.(nbytes + cp_nbytes) in
          fn ~seq:seq' (Uint.succ cindex) nbytes'
        end
    end in
    fn ~seq (Uint.kv 0) (Uint.kv 0)

  let init ?blength clength ~f =
    let blength = match blength with
      | None -> blength_of_seq clength ~seq:(Uint.kv 0)
          ~f:(fun seq ->
            (f seq), (Uint.succ seq)
          )
      | Some blength -> blength
    in
    of_string String_of_indexed.(to_string (init ~f blength))

  let of_codepoint codepoint =
    init (Uint.kv 1) ~f:(fun _ -> codepoint)

  module String_of_list_common = struct
    type t = {
      codepoints: codepoint list;
      blength: uint;
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
      let nbytes = Utf8.(length (of_codepoint codepoint)) in
      let blength = Uint.(t.blength - nbytes) in
      codepoint, {codepoints; blength}
  end

  module String_of_list = struct
    include String_of_list_common
    include Seq.Codepoint.Make(String_of_list_common)
  end

  module String_of_list_rev = struct
    include String_of_list_common
    include Seq.Codepoint.Make_rev(String_of_list_common)
  end

  let blength_of_list clength codepoints =
    blength_of_seq clength ~seq:codepoints ~f:(fun seq ->
      match seq with
      | cp :: seq' -> cp, seq'
      | [] -> not_reached ()
    )

  let of_list ?blength ?clength codepoints =
    let clength = match clength with
      | None -> List.length codepoints
      | Some clength -> clength
    in
    let blength = match blength with
      | None -> blength_of_list clength codepoints
      | Some blength -> blength
    in
    of_string String_of_list.(to_string (init codepoints blength))

  let of_list_rev ?blength ?clength codepoints_rev =
    let clength = match clength with
      | None -> List.length codepoints_rev
      | Some clength -> clength
    in
    let blength = match blength with
      | None -> blength_of_list clength codepoints_rev
      | Some blength -> blength
    in
    of_string String_of_list_rev.(to_string (init codepoints_rev blength))

  let of_array ?blength codepoints =
    init ?blength (Array.length codepoints) ~f:(fun i ->
      Array.get codepoints i
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
          slice.base

        let tl slice =
          slice.past

        let succ = Cursor.succ

        let pred = Cursor.pred

        let lget = Cursor.lget

        let rget = Cursor.rget
      end
      include T
      include Cmpable.Make(T)
    end

    let cmp_elm = Codepoint.cmp

    let length = clength
  end
  include Container_common.Make_mono_fold(U)
  include Container_common.Make_mono_mem(U)
  include Container_array.Make_mono_array(U)

  module String_mapi = struct
    module T = struct
      type t = {
        f: uint -> codepoint -> codepoint;
        cursor: cursor;
        cindex: uint;
        blength: uint;
      }

      let init slice ~f blength =
        {
          f;
          cursor=slice.base;
          cindex=Uint.kv 0;
          blength
        }

      let length t =
        t.blength

      let next t =
        let codepoint = Cursor.rget t.cursor in
        let codepoint' = t.f t.cindex codepoint in
        let utf8_length = Utf8.(length (of_codepoint codepoint')) in
        let cursor' = Cursor.succ t.cursor in
        let cindex' = Uint.succ t.cindex in
        let blength' = Uint.(t.blength - utf8_length) in
        let t' = {t with cursor=cursor';
                         cindex=cindex';
                         blength=blength'} in
        codepoint', t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_map t ~f =
    foldi t ~init:(Uint.kv 0, false) ~f:(fun i (blength, modified) codepoint ->
      let codepoint' = f i codepoint in
      let modified' = modified || Codepoint.(codepoint' <> codepoint) in
      Uint.(blength + Utf8.(length (of_codepoint codepoint'))), modified'
    )

  let map t ~f =
    let f' _ codepoint = begin
      f codepoint
    end in
    let blength, modified = blength_of_map t ~f:f' in
    match modified with
    | false -> t
    | true -> of_string String_mapi.(to_string (init t blength ~f:f'))

  let mapi t ~f =
    let blength, modified = blength_of_map t ~f in
    match modified with
    | false -> t
    | true -> of_string String_mapi.(to_string (init t blength ~f))

  let tr ~target ~replacement t =
    let f _ codepoint = begin
      if Codepoint.(codepoint = target) then replacement
      else codepoint
    end in
    let blength, modified = blength_of_map t ~f in
    match modified with
    | false -> t
    | true -> of_string String_mapi.(to_string (init t blength ~f))

  let filter t ~f =
    let codepoints, modified = fold_right t ~init:([], false)
        ~f:(fun codepoint (codepoints, modified) ->
          if f codepoint then codepoint :: codepoints, modified
          else codepoints, true
        ) in
    match modified with
    | false -> t
    | true -> of_list codepoints

  module String_concat = struct
    module T = struct
      type outer = t
      type source =
      | Sep
      | Str
      type t = {
        sep: outer;
        slices: outer list;
        source: source;
        blength: uint;
      }

      let init sep slices blength =
        {sep=sep; slices; source=Str; blength}

      let length t =
        t.blength

      let next t =
        let rec fn t = begin
          match t.source with
          | Sep -> begin
              match Uint.((blength t.sep) = (kv 0)) with
              | true -> fn {t with source=Str}
              | false -> begin
                  let blength' = Uint.(t.blength - (blength t.sep)) in
                  t.sep, {t with source=Str; blength=blength'}
                end
            end
          | Str -> begin
              match t.slices with
              | [] -> not_reached ()
              | slice :: slices' -> begin
                  let blength' = Uint.(t.blength - (blength slice)) in
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
    let _, blength = List.fold slices ~init:((Uint.kv 0), (Uint.kv 0))
        ~f:(fun (i, len) slice ->
          let i' = Uint.succ i in
          let sep_len = match i with
            | i when Uint.(i = (kv 0)) -> (Uint.kv 0)
            | _ -> blength sep
          in
          let len' = Uint.(sep_len + len + (blength slice)) in
          i', len'
        ) in
    match Uint.((length sep) = (kv 0)), Uint.to_int (List.length slices) with
    | _, 0 -> of_string ""
    | true, 1 -> List.hd slices
    | _ -> of_string (String_concat.to_string
          (String_concat.init sep slices blength))

  let concat_rev ?(sep=(of_string "")) slices_rev =
    let slices, blength = List.fold slices_rev ~init:([], (Uint.kv 0))
        ~f:(fun (slices, len) slice ->
          let slices' = slice :: slices in
          let sep_len = match slices with
            | [] -> Uint.kv 0
            | _ -> blength sep
          in
          let len' = Uint.(sep_len + len + (blength slice)) in
          slices', len'
        ) in
    match Uint.((length sep) = (kv 0)), Uint.to_int (List.length slices) with
    | _, 0 -> of_string ""
    | true, 1 -> List.hd slices
    | _ -> of_string (String_concat.to_string
          (String_concat.init sep slices blength))

  let concat_map ?sep t ~f =
    (* Iterate in reverse order to generate a list of slices that can then be
     * passed to concat. *)
    let modified, slices = fold_right t ~init:(false, [])
        ~f:(fun cp (modified, slices) ->
          let slice = f cp in
          let modified' = modified
                          || Uint.((blength slice)
                            <> Utf8.(length (of_codepoint cp)))
                          || Codepoint.(Cursor.(rget slice.base) <> cp) in
          let slices' = slice :: slices in
          modified', slices'
        ) in
    match modified, sep with
    | false, None -> t
    | _ -> concat ?sep slices

  let escaped t =
    concat_map t ~f:(fun codepoint ->
      match codepoint with
        | cp when Codepoint.(cp = nul) -> of_string "\\0"
        | cp when Codepoint.(cp = soh) -> of_string "\\x01"
        | cp when Codepoint.(cp = stx) -> of_string "\\x02"
        | cp when Codepoint.(cp = etx) -> of_string "\\x03"
        | cp when Codepoint.(cp = eot) -> of_string "\\x04"
        | cp when Codepoint.(cp = enq) -> of_string "\\x05"
        | cp when Codepoint.(cp = ack) -> of_string "\\x06"
        | cp when Codepoint.(cp = bel) -> of_string "\\a"
        | cp when Codepoint.(cp = bs) -> of_string "\\b"
        | cp when Codepoint.(cp = ht) -> of_string "\\t"
        | cp when Codepoint.(cp = nl) -> of_string "\\n"
        | cp when Codepoint.(cp = vt) -> of_string "\\v"
        | cp when Codepoint.(cp = ff) -> of_string "\\f"
        | cp when Codepoint.(cp = cr) -> of_string "\\r"
        | cp when Codepoint.(cp = so) -> of_string "\\x0e"
        | cp when Codepoint.(cp = si) -> of_string "\\x0f"
        | cp when Codepoint.(cp = dle) -> of_string "\\x10"
        | cp when Codepoint.(cp = dc1) -> of_string "\\x11"
        | cp when Codepoint.(cp = dc2) -> of_string "\\x12"
        | cp when Codepoint.(cp = dc3) -> of_string "\\x13"
        | cp when Codepoint.(cp = dc4) -> of_string "\\x14"
        | cp when Codepoint.(cp = nak) -> of_string "\\x15"
        | cp when Codepoint.(cp = syn) -> of_string "\\x16"
        | cp when Codepoint.(cp = etb) -> of_string "\\x17"
        | cp when Codepoint.(cp = can) -> of_string "\\x18"
        | cp when Codepoint.(cp = em) -> of_string "\\x19"
        | cp when Codepoint.(cp = sub) -> of_string "\\x1a"
        | cp when Codepoint.(cp = esc) -> of_string "\\x1b"
        | cp when Codepoint.(cp = fs) -> of_string "\\x1c"
        | cp when Codepoint.(cp = gs) -> of_string "\\x1d"
        | cp when Codepoint.(cp = rs) -> of_string "\\x1e"
        | cp when Codepoint.(cp = us) -> of_string "\\x1f"
        | cp when Codepoint.(cp = (of_char '"')) -> of_string "\\\""
        | cp when Codepoint.(cp = (of_char '\\')) -> of_string "\\\\"
        | cp when Codepoint.(cp = del) -> of_string "\\x7f"
        | _ -> of_codepoint codepoint
      )

  module String_rev = struct
    module T = struct
      type outer = t
      type t = {
        slice: outer;
        cursori: cursori;
      }

      let init slice =
        {slice; cursori=Cursori.hd (string slice)}

      let length t =
        Uint.((clength t.slice) - (Cursori.cindex t.cursori))

      let next t =
        let codepoint = Cursori.rget t.cursori in
        let t' = {t with cursori=(Cursori.succ t.cursori)} in
        codepoint, t'
    end
    include T
    include Seq.Codepoint.Make_rev(T)
  end

  let rev t =
    of_string String_rev.(to_string (init t))

  let lfind t codepoint =
    let rec fn cursor = begin
      match Cursor.(cursor = t.past) with
      | true -> None
      | false -> begin
          let cp = Cursor.rget cursor in
          match Codepoint.(cp = codepoint) with
          | true -> Some cursor
          | false -> fn (Cursor.succ cursor)
        end
    end in
    fn t.base

  let lfind_hlt t codepoint =
    match lfind t codepoint with
    | None -> halt "Codepoint not found"
    | Some cursor -> cursor

  let contains t codepoint =
    match lfind t codepoint with
    | None -> false
    | Some _ -> true

  let rfind t codepoint =
    let rec fn cursor = begin
      match Cursor.(cursor = t.base) with
      | true -> None
      | false -> begin
          let cp = Cursor.lget cursor in
          let cursor' = Cursor.pred cursor in
          match Codepoint.(cp = codepoint) with
          | true -> Some cursor'
          | false -> fn cursor'
        end
    end in
    fn t.past

  let rfind_hlt t codepoint =
    match rfind t codepoint with
    | None -> halt "Codepoint not found"
    | Some codepoint -> codepoint

  module Pattern = struct
    type outer_string = outer
    type outer = t
    type t = {
      (* Pattern slice. *)
      p: outer_string;
      (* Auxilliary data structure that enables Knuth-Morris-Pratt (KMP) pattern
       * matching.  The cursors correspond to the codepoints of p.  Each cursor
       * points to the end of the longest pattern prefix that has an equivalent
       * suffix in the substring ending just prior to the corresponding
       * codepoint index. *)
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
                   Uint.is_positive (Cursori.cindex k)) with
            | true ->
              let k' = Array.get pi (Uint.pred (Cursori.cindex k)) in
              compute_pi ~p ~k:k' ~q ~pi
            | false -> begin
                let k' = match (k_eq_q ~k ~q) with
                  | true -> Cursori.(succ k)
                  | false -> k
                in
                let q' = Cursori.succ q in
                Array.set_inplace pi (Cursori.cindex q) k';
                compute_pi ~p ~k:k' ~q:q' ~pi
              end
          end
      end in
      let pi = match Uint.(Cursori.cindex m = (kv 0)) with
        | true -> [||]
        | false -> begin
            let k = Cursori.hd s in
            let q = Cursori.succ k in
            let pi = Array.init (Cursori.cindex m) ~f:(fun _ -> k) in
            compute_pi ~p:s ~k ~q ~pi
          end
      in
      {p=s; pi}

    (* XXX Refator to use Format. *)
    let pretty_print t =
      let open Format in
      printf " p=\"%s\"\npi=[" t.p;
      Array.iter t.pi ~f:(fun elm ->
        let () = printf "%a" pp (Cursori.cindex elm) in
        ()
      );
      let () = printf "]\n" in
      ()

    let find_impl ?max_matches t ~may_overlap ~in_ =
      let past = in_.past in
      let m = Cursori.tl t.p in
      let rec fn ~q ~i matches nmatches = begin
        match max_matches, Cursori.(q = m) with
        | Some n, _ when Uint.(nmatches = n) -> List.rev matches
        | _, true -> begin
            let cursor = (Cursor.at (string in_)
                ~bindex:Uint.((Cursor.bindex in_.base)
                    + (Cursor.bindex i) - (Cursori.bindex m))) in
            let matches' = cursor :: matches in
            let nmatches' = Uint.succ nmatches in
            match may_overlap, Uint.((Cursori.bindex m) = (kv 0)),
              Cursor.(i = past) with
            (* Empty pattern; terminate scanning or advance i to avoid multiple
             * matches at the same position. *)
            | _, true, true -> List.rev matches'
            | _, true, false -> begin
                let q' = Cursori.hd t.p in
                let i' = Cursor.succ i in
                fn ~q:q' ~i:i' matches' nmatches'
              end
            (* Attempt overlapping match. *)
            | true, false, _ -> begin
                let q' =
                  Array.get t.pi (Uint.pred (Cursori.cindex q)) in
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
                match (not (q_eq_i ~q ~i)) &&
                      (Uint.is_positive (Cursori.cindex q)) with
                | true -> begin
                    let q' =
                      Array.get t.pi (Uint.pred (Cursori.cindex q)) in
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
      let i = in_.base in
      fn ~q ~i [] (Uint.kv 0)

    let find t ~in_ =
      let cursors =
        find_impl t ~max_matches:(Uint.kv 1) ~may_overlap:false ~in_ in
      match cursors with
      | [] -> None
      | cursor :: [] -> Some cursor
      | _ :: _ -> not_reached ()

    let find_hlt t ~in_ =
      match find t ~in_ with
      | None -> halt "No match"
      | Some cursor -> cursor

    let find_all t ~may_overlap ~in_ =
      find_impl t ~may_overlap ~in_

    module String_pattern_replace = struct
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
          slice: slice;
          source: source;
          blength: uint;
        }

        let init ~pattern ~in_ ~with_ ~at =
          let in_cursor, slice, at', source = match at with
            | [] -> in_.base, in_, at, In
            | cursor :: at' when Cursor.(cursor = in_.base) ->
              let in_cursor = (Cursor.at cursor.string ~bindex:Uint.(
                cursor.bindex + (string_blength pattern))) in
              let slice = with_ in
              in_cursor, slice, at', With
            | cursor :: _ -> begin
                let slice = slice_of_cursors ~base:in_.base ~past:cursor in
                in_.past, slice, at, In
              end
          in
          let ncursors = List.length at in
          let blength = Uint.((blength in_) +
                (ncursors * ((blength with_) - (string_blength pattern)))) in
          {in_; pattern; with_; at=at'; in_cursor; slice; source; blength}

        let length t =
          t.blength

        let next t =
          let blength' = Uint.(t.blength - ((t.slice.past.bindex) -
                  t.slice.base.bindex)) in
          let t' = match t.source with
            | In -> begin
                let in_cursor', slice', at' = match t.at with
                  | [] -> begin
                      let slice' = slice_of_cursors ~base:t.in_.past
                          ~past:t.in_.past in
                      t.in_.past, slice', []
                    end
                  | cursor :: at' -> begin
                      let in_cursor' = Cursor.seek cursor
                          (Uint.to_int (string_clength t.pattern)) in
                      let slice' = t.with_ in
                      in_cursor', slice', at'
                    end
                in
                {t with at=at';
                        in_cursor=in_cursor';
                        slice=slice';
                        source=With;
                        blength=blength'}
              end
            | With -> begin
                let slice' = match t.at with
                  | [] ->
                    slice_of_cursors ~base:t.in_cursor ~past:t.in_.past
                  | cursor :: _ ->
                    slice_of_cursors ~base:t.in_cursor ~past:cursor
                in
                {t with slice=slice';
                        source=In;
                        blength=blength'}
              end
          in t.slice, t'
      end
      include T
      include Seq.Slice.Make(T)
    end

    let replace_first t ~in_ ~with_ =
      match find t ~in_ with
      | None -> in_
      | Some cursor -> of_string (String_pattern_replace.(to_string
            (init ~pattern:t.p ~in_ ~with_ ~at:[cursor])))

    let replace_all t ~in_ ~with_ =
      match find_all t ~may_overlap:false ~in_ with
      | [] -> in_
      | cursors -> of_string (String_pattern_replace.(to_string
            (init ~pattern:t.p ~in_ ~with_ ~at:cursors)))
  end

  let prefix_tl t ~prefix =
    let rec fn t_cursor prefix_cursor = begin
      let end_of_t = Cursor.(t_cursor = t.past) in
      let end_of_prefix = Cursor.(prefix_cursor = prefix.past) in
      match end_of_t, end_of_prefix with
      | true, false -> None
      | _, true -> Some t_cursor
      | false, false -> begin
          match Codepoint.((Cursor.rget t_cursor)
              = (Cursor.rget prefix_cursor)) with
          | false -> None
          | true -> fn (Cursor.succ t_cursor) (Cursor.succ prefix_cursor)
        end
    end in
    fn t.base prefix.base

  let is_prefix t ~prefix =
    match prefix_tl t ~prefix with
    | None -> false
    | Some _ -> true

  let suffix_hd t ~suffix =
    let rec fn t_cursor suffix_cursor = begin
      let beg_of_t = Cursor.(t_cursor = t.base) in
      let beg_of_suffix = Cursor.(suffix_cursor = suffix.base) in
      match beg_of_t, beg_of_suffix with
      | true, false -> None
      | _, true -> Some t_cursor
      | false, false -> begin
          match Codepoint.((Cursor.lget t_cursor)
            = (Cursor.lget suffix_cursor)) with
          | false -> None
          | true -> fn (Cursor.pred t_cursor) (Cursor.pred suffix_cursor)
        end
    end in
    fn t.past suffix.past

  let is_suffix t ~suffix =
    match suffix_hd t ~suffix with
    | None -> false
    | Some _ -> true

  let prefix t n =
    let base = t.base in
    let past = match Uint.((clength t) < n) with
      | true -> t.past
      | false -> Cursor.(seek base (Uint.to_int n))
    in
    of_cursors ~base ~past

  let suffix t n =
    let past = t.past in
    let base = match Uint.((clength t) < n) with
      | true -> t.base
      | false -> Cursor.(seek past (-(Uint.to_int n)))
    in
    of_cursors ~base ~past

  let chop_prefix t ~prefix =
    match prefix_tl t ~prefix with
    | None -> None
    | Some base -> Some (of_cursors ~base ~past:t.past)

  let chop_prefix_hlt t ~prefix =
    match chop_prefix t ~prefix with
    | None -> halt "Not a prefix"
    | Some slice -> slice

  let chop_suffix t ~suffix =
    match suffix_hd t ~suffix with
    | None -> None
    | Some past -> Some (of_cursors ~base:t.base ~past)

  let chop_suffix_hlt t ~suffix =
    match chop_suffix t ~suffix with
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
    of_cursors ~base ~past:(past t)

  let rstrip ?drop t =
    let drop = match drop with
      | None -> drop_whitespace
      | Some drop -> drop
    in
    let past = strip_past drop t in
    of_cursors ~base:(base t) ~past

  let strip ?drop t =
    let drop = match drop with
      | None -> drop_whitespace
      | Some drop -> drop
    in
    let base = strip_base drop t in
    let past = strip_past drop t in
    of_cursors ~base ~past

  let split_fold_until t ~init ~on ~f =
    let rec fn base past accum = begin
      match Cursor.(past = t.past) with
      | true -> begin
          let slice = of_cursors ~base ~past in
          let accum', _ = f accum slice in
          accum'
        end
      | false -> begin
          match on (Cursor.rget past) with
          | true -> begin
              let base' = Cursor.succ past in
              let past' = base' in
              let slice = of_cursors ~base ~past in
              let accum', until = f accum slice in
              match until with
              | true -> accum'
              | false -> fn base' past' accum'
            end
          | false -> fn base (Cursor.succ past) accum
        end
    end in
    fn t.base t.base init

  let split_fold t ~init ~on ~f =
    split_fold_until t ~init ~on ~f:(fun accum slice -> (f accum slice), false)

  let split_fold_right_until t ~init ~on ~f =
    let rec fn base past accum = begin
      match Cursor.(base = t.base) with
      | true -> begin
          let slice = of_cursors ~base ~past in
          let accum', _ = f slice accum in
          accum'
        end
      | false -> begin
          match on (Cursor.lget base) with
          | true -> begin
              let base' = Cursor.pred base in
              let past' = base' in
              let slice = of_cursors ~base ~past in
              let accum', until = f slice accum in
              match until with
              | true -> accum'
              | false -> fn base' past' accum'
            end
          | false -> fn (Cursor.pred base) past accum
        end
    end in
    fn t.past t.past init

  let split_fold_right t ~init ~on ~f =
    split_fold_right_until t ~init ~on ~f:(fun slice accum ->
      (f slice accum), false
    )

  let lines_fold t ~init ~f =
    let rec fn base past cr_seen accum = begin
      match Cursor.(past = t.past) with
      | true -> begin
          let slice = of_cursors ~base ~past in
          f accum slice
        end
      | false -> begin
          match Cursor.rget past with
          | cp when Codepoint.(cp = nl) -> begin
              let base' = Cursor.succ past in
              let past' = base' in
              let slice = match cr_seen with
                | false -> of_cursors ~base ~past
                | true -> of_cursors ~base ~past:(Cursor.pred past)
              in
              let accum' = f accum slice in
              fn base' past' false accum'
            end
          | cp when Codepoint.(cp = cr) -> fn base (Cursor.succ past) true accum
          | _ ->  fn base (Cursor.succ past) false accum
        end
    end in
    fn t.base t.base false init

  let lines_fold_right t ~init ~f =
    let rec fn base past nl_seen accum = begin
      match Cursor.(base = t.base) with
      | true -> begin
          match nl_seen with
          | false -> begin
              let slice = of_cursors ~base ~past in
              f slice accum
            end
          | true -> begin
              let slice = of_cursors ~base:(Cursor.succ base) ~past in
              let accum' = f slice accum in
              let empty_slice = of_cursors ~base ~past:base in
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
                | cp when Codepoint.(cp = cr) ->
                  (Cursor.pred base), (Cursor.pred base), false
                | _ -> base, base, false
              in
              let slice = of_cursors ~base:(Cursor.succ base) ~past in
              let accum' = f slice accum in
              fn base' past' nl_seen' accum'
            end
        end
    end in
    fn t.past t.past false init

  let lsplit2 t ~on =
    split_fold_until t ~init:None ~on:(fun codepoint ->
      Codepoint.(codepoint = on)
    ) ~f:(fun _ slice ->
      let base = Cursor.succ (past slice) in
      let past = past t in
      let slice2 = of_cursors ~base ~past in
      (Some (slice, slice2)), true
    )

  let lsplit2_hlt t ~on =
    match lsplit2 t ~on with
    | None -> halt "No split performed"
    | Some slice -> slice

  let rsplit2 t ~on =
    split_fold_right_until t ~init:None ~on:(fun codepoint ->
      Codepoint.(codepoint = on)
    ) ~f:(fun slice _ ->
      let base, past = (base t), (Cursor.pred (base slice)) in
      let slice0 = of_cursors ~base ~past in
      (Some (slice0, slice)), true
    )

  let rsplit2_hlt t ~on =
    match rsplit2 t ~on with
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

let init ?blength clength ~f =
  Slice.to_string (Slice.init ?blength clength ~f)

let of_codepoint codepoint =
  Slice.(to_string (of_codepoint codepoint))

let of_list ?blength ?clength codepoints =
  Slice.to_string (Slice.of_list ?blength ?clength codepoints)

let of_list_rev ?blength ?clength codepoints_rev =
  Slice.to_string (Slice.of_list_rev ?blength ?clength codepoints_rev)

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
include Container_common.Make_mono_fold(U)
include Container_common.Make_mono_mem(U)
include Container_array.Make_mono_array(U)

let map t ~f =
  Slice.(to_string (map (of_string t) ~f))

let mapi t ~f =
  Slice.(to_string (mapi (of_string t) ~f))

let tr ~target ~replacement t =
  Slice.(to_string (tr ~target ~replacement (of_string t)))

let filter t ~f =
  Slice.(to_string (filter (of_string t) ~f))

let concat ?(sep="") strings =
  let slices_rev = List.fold strings ~init:[]
      ~f:(fun accum s -> (Slice.of_string s) :: accum) in
  Slice.(to_string (concat_rev ~sep:(of_string sep) slices_rev))

let concat_rev ?(sep="") strings_rev =
  let slices = List.fold strings_rev ~init:[]
      ~f:(fun accum s -> (Slice.of_string s) :: accum) in
  Slice.(to_string (concat ~sep:(of_string sep) slices))

let concat_map ?sep t ~f =
  let f = (fun cp -> Slice.of_string (f cp)) in
  Slice.to_string (match sep with
    | None -> Slice.(concat_map (of_string t) ~f)
    | Some sep -> Slice.(concat_map ~sep:(of_string sep) (of_string t) ~f)
  )

let escaped t =
  Slice.(to_string (escaped (of_string t)))

let rev t =
  Slice.(to_string (rev (of_string t)))

let ( ^ ) t0 t1 =
  concat [t0; t1]

let lfind ?base ?past t codepoint =
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = match past with
    | None -> Cursor.tl t
    | Some cursor -> cursor
  in
  Slice.lfind (Slice.of_cursors ~base ~past) codepoint

let lfind_hlt ?base ?past t codepoint =
  match lfind ?base ?past t codepoint with
  | None -> halt "Codepoint not found"
  | Some cursor -> cursor

let contains ?base ?past t codepoint =
  match lfind ?base ?past t codepoint with
  | None -> false
  | Some _ -> true

let rfind ?base ?past t codepoint =
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = match past with
    | None -> Cursor.tl t
    | Some cursor -> cursor
  in
  Slice.rfind (Slice.of_cursors ~base ~past) codepoint

let rfind_hlt ?base ?past t codepoint =
  match rfind ?base ?past t codepoint with
  | None -> halt "Codepoint not found"
  | Some codepoint -> codepoint

let substr_find ?base t ~pattern =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_cursors ~base ~past in
  Slice.Pattern.find p ~in_

let substr_find_hlt ?base t ~pattern =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_cursors ~base ~past in
  Slice.Pattern.find_hlt p ~in_

let substr_find_all t ~may_overlap ~pattern =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  Slice.Pattern.find_all p ~may_overlap ~in_:(Slice.of_string t)

let substr_replace_first ?base t ~pattern ~with_ =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_cursors ~base ~past in
  let with_ = Slice.of_string with_ in
  Slice.(to_string (Pattern.replace_first p ~in_ ~with_))

let substr_replace_all t ~pattern ~with_ =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let in_ = Slice.of_string t in
  let with_ = Slice.of_string with_ in
  Slice.(to_string (Pattern.replace_all p ~in_ ~with_))

let is_prefix t ~prefix =
  Slice.is_prefix (Slice.of_string t) ~prefix:(Slice.of_string prefix)

let is_suffix t ~suffix =
  Slice.is_suffix (Slice.of_string t) ~suffix:(Slice.of_string suffix)

let pare ~base ~past =
  match Cursor.(base = (hd (container base)))
                && Cursor.(past = (tl (container past))) with
  | true -> Cursor.container base
  | false -> Slice.to_string (Slice.of_cursors ~base ~past)

let prefix t n =
  Slice.(to_string (prefix (of_string t) n))

let suffix t n =
  Slice.(to_string (suffix (of_string t) n))

let chop_prefix t ~prefix =
  let slice_opt = Slice.chop_prefix (Slice.of_string t)
      ~prefix:(Slice.of_string prefix) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_prefix_hlt t ~prefix =
  Slice.to_string (Slice.chop_prefix_hlt (Slice.of_string t)
      ~prefix:(Slice.of_string prefix))

let chop_suffix t ~suffix =
  let slice_opt = Slice.chop_suffix (Slice.of_string t)
      ~suffix:(Slice.of_string suffix) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_suffix_hlt t ~suffix =
  Slice.to_string (Slice.chop_suffix_hlt (Slice.of_string t)
      ~suffix:(Slice.of_string suffix))

let lstrip ?drop t =
  Slice.(to_string (lstrip ?drop (of_string t)))

let rstrip ?drop t =
  Slice.(to_string (rstrip ?drop (of_string t)))

let strip ?drop t =
  Slice.(to_string (strip ?drop (of_string t)))

let split t ~f =
  Slice.split_fold_right (Slice.of_string t) ~init:[] ~on:f
    ~f:(fun slice strings -> (Slice.to_string slice) :: strings)

let split_rev t ~f =
  Slice.split_fold (Slice.of_string t) ~init:[] ~on:f
    ~f:(fun strings slice -> (Slice.to_string slice) :: strings)

let split_lines t =
  Slice.lines_fold_right (Slice.of_string t) ~init:[] ~f:(fun slice lines ->
    (Slice.to_string slice) :: lines
  )

let split_lines_rev t =
  Slice.lines_fold (Slice.of_string t) ~init:[] ~f:(fun lines slice ->
    (Slice.to_string slice) :: lines
  )

let lsplit2 t ~on =
  match Slice.lsplit2 (Slice.of_string t) ~on with
  | None -> None
  | Some (slice, slice2) ->
    Some ((Slice.to_string slice), (Slice.to_string slice2))

let lsplit2_hlt t ~on =
  let slice, slice2 = Slice.lsplit2_hlt (Slice.of_string t) ~on in
  (Slice.to_string slice), (Slice.to_string slice2)

let rsplit2 t ~on =
  match Slice.rsplit2 (Slice.of_string t) ~on with
  | None -> None
  | Some (slice, slice2) ->
    Some ((Slice.to_string slice), (Slice.to_string slice2))

let rsplit2_hlt t ~on =
  let slice, slice2 = Slice.rsplit2_hlt (Slice.of_string t) ~on in
  (Slice.to_string slice), (Slice.to_string slice2)

module O = struct
  module T = struct
    type nonrec t = t

    let cmp = cmp
  end
  include T
  include Cmpable.Make(T)
end

(*******************************************************************************
 * Begin tests.
 *)

let%expect_test "hash_fold" =
  let open Format in

  let s = "hello" in
  let h = Hash.(t_of_state (hash_fold (Hash.state_of_uint (Uint.kv 0)) s)) in
  printf "hash_fold \"%s\"=%a\n" s Hash.pp_x h;

  [%expect{|
    hash_fold "hello"=0x321f6e00
    |}]

let%expect_test "cmp" =
  let open Format in
  let strs = [
    "";
    "a";
    "aa";
    "ab";
    "aa";
    "a";
    "";
  ] in
  let rec fn s strs = begin
    match strs with
    | [] -> ()
    | hd :: tl -> begin
        let () = List.iter strs ~f:(fun s2 ->
          printf "cmp \"%s\" \"%s\" -> %s\n"
            s s2 (Sexplib.Sexp.to_string (Cmp.sexp_of_t (cmp s s2)))
        ) in
        fn hd tl
      end
  end in
  let hd, tl = match strs with
    | hd :: tl -> hd, tl
    | [] -> not_reached ()
  in
  fn hd tl;

  [%expect{|
    cmp "" "a" -> Lt
    cmp "" "aa" -> Lt
    cmp "" "ab" -> Lt
    cmp "" "aa" -> Lt
    cmp "" "a" -> Lt
    cmp "" "" -> Eq
    cmp "a" "aa" -> Lt
    cmp "a" "ab" -> Lt
    cmp "a" "aa" -> Lt
    cmp "a" "a" -> Eq
    cmp "a" "" -> Gt
    cmp "aa" "ab" -> Lt
    cmp "aa" "aa" -> Eq
    cmp "aa" "a" -> Gt
    cmp "aa" "" -> Gt
    cmp "ab" "aa" -> Gt
    cmp "ab" "a" -> Gt
    cmp "ab" "" -> Gt
    cmp "aa" "a" -> Gt
    cmp "aa" "" -> Gt
    cmp "a" "" -> Gt
    |}]

let%expect_test "sexp" =
  let open Format in
  let s = "hello" in
  let sexp = sexp_of_t s in
  let t = t_of_sexp sexp in

  printf "s=\"%s\", sexp=%s, t=\"%s\"\n" s (Sexplib.Sexp.to_string sexp) t;

  [%expect{|
    s="hello", sexp=hello, t="hello"
    |}]

let%expect_test "string" =
  let open Format in

  let s = "hello" in
  let s2 = of_string s in
  let s3 = to_string s2 in

  printf "s=\"%s\", s2=\"%s\", s3=\"%s\"\n" s s2 s3;

  [%expect{|
    s="hello", s2="hello", s3="hello"
    |}]

let%expect_test "length" =
  let open Format in
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    printf "s=\"%s\", blength=%a, clength=%a, is_empty=%B\n"
      s
      pp (blength s)
      pp (clength s)
      (is_empty s)
  );

  [%expect{|
    s="", blength=0, clength=0, is_empty=true
    s="<_>", blength=3, clength=3, is_empty=false
    s="«»", blength=4, clength=2, is_empty=false
    s="‡", blength=3, clength=1, is_empty=false
    s="𐆗", blength=4, clength=1, is_empty=false
    |}]

let%expect_test "get" =
  let open Format in
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    let rec fn i = begin
      match Uint.(i = (blength s)) with
      | true -> ()
      | false -> begin
          printf " %#02x" (Byte.to_int (get s i));
          fn (Uint.succ i)
        end
    end in
    printf "s=\"%s\":" s;
    fn (Uint.kv 0);
    printf "\n";
  );

  [%expect{|
    s="":
    s="<_>": 0x3c 0x5f 0x3e
    s="«»": 0xc2 0xab 0xc2 0xbb
    s="‡": 0xe2 0x80 0xa1
    s="𐆗": 0xf0 0x90 0x86 0x97
    |}]

let%expect_test "cursor" =
  let open Format in
  let test_fwd s = begin
    let rec fn cursor i_prev = begin
      match Cursor.(cursor = (tl s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursor.bindex cursor in
          assert Cursor.((at s ~bindex:i) = cursor);
          let () = if Uint.(i_prev <> max_value) then
            for j = 0 to Uint.(to_int (i - i_prev - (kv 1))) do
              let j = Uint.of_int j in
              assert Cursor.((near s ~bindex:Uint.(i_prev + j))
                  = (at s ~bindex:i_prev));
            done
          in
          printf " %a=%s" pp i (of_codepoint (Cursor.rget cursor));
          fn (Cursor.succ cursor) i
        end
    end in
    printf "cursor fwd \"%s\":" s;
    fn (Cursor.hd s);
  end in
  let test_rev s = begin
    let rec fn cursor i_prev = begin
      match Cursor.(cursor = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursor.bindex cursor in
          assert Cursor.((at s ~bindex:i) = cursor);
          let () = if Uint.(i_prev <> max_value) then
            for j = 0 to Uint.(to_int (i_prev - i - (kv 1))) do
              let j = Uint.of_int j in
              assert Cursor.((near s ~bindex:Uint.(i + j)) = (at s ~bindex:i));
            done
          in
          printf " %a=%s" pp i (of_codepoint (Cursor.lget cursor));
          fn (Cursor.pred cursor) i
        end
    end in
    printf "cursor rev \"%s\":" s;
    fn (Cursor.tl s);
  end in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  List.iter strs ~f:(fun s ->
    test_fwd s Uint.max_value;
    test_rev s Uint.max_value;
  );

  [%expect{|
    cursor fwd "":
    cursor rev "":
    cursor fwd "<_>«‡𐆗»[_]": 0=< 1=_ 2=> 3=« 5=‡ 8=𐆗 12=» 14=[ 15=_ 16=]
    cursor rev "<_>«‡𐆗»[_]": 17=] 16=_ 15=[ 14=» 12=𐆗 8=‡ 5=« 3=> 2=_ 1=<
    |}]

let%expect_test "cursori" =
  let open Format in

  let test_fwd s = begin
    let rec fn cursori = begin
      match Cursori.(cursori = (tl s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursori.cindex cursori in
          assert Cursori.((at s ~cindex:i) = cursori);
          printf " %a=%s" pp i (of_codepoint (Cursori.rget cursori));
          fn (Cursori.succ cursori)
        end
    end in
    printf "cursori fwd \"%s\":" s;
    fn (Cursori.hd s);
  end in

  let test_rev s = begin
    let rec fn cursori = begin
      match Cursori.(cursori = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursori.cindex cursori in
          assert Cursori.((at s ~cindex:i) = cursori);
          printf " %a=%s" pp i (of_codepoint (Cursori.lget cursori));
          fn (Cursori.pred cursori)
        end
    end in
    printf "cursori rev \"%s\":" s;
    fn (Cursori.tl s);
  end in

  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  List.iter strs ~f:(fun s ->
    test_fwd s;
    test_rev s;
  );

  [%expect{|
    cursori fwd "":
    cursori rev "":
    cursori fwd "<_>«‡𐆗»[_]": 0=< 1=_ 2=> 3=« 4=‡ 5=𐆗 6=» 7=[ 8=_ 9=]
    cursori rev "<_>«‡𐆗»[_]": 10=] 9=_ 8=[ 7=» 6=𐆗 5=‡ 4=« 3=> 2=_ 1=<
    |}]

let%expect_test "fold_until" =
  let open Format in
  let test_fold_until s = begin
    printf "fold_until \"%s\" ->" s;
    let () = fold_until s ~init:() ~f:(fun _ cp ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %s" (of_codepoint cp);
      (), until
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold_until;

  [%expect{|
    fold_until "" ->
    fold_until "abcde" -> a b c
    |}]

let%expect_test "fold_right_until" =
  let open Format in
  let test_fold_right_until s = begin
    printf "fold_right_until \"%s\" ->" s;
    let () = fold_right_until s ~init:() ~f:(fun cp _ ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %s" (of_codepoint cp);
      (), until
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold_right_until;

  [%expect{|
    fold_right_until "" ->
    fold_right_until "abcde" -> e d c
    |}]

let%expect_test "foldi_until" =
  let open Format in
  let test_foldi_until s = begin
    printf "foldi_until \"%s\" ->" s;
    let () = foldi_until s ~init:() ~f:(fun i _ cp ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %d:%s" (Uint.to_int i) (of_codepoint cp);
      (), until
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi_until;

  [%expect{|
    foldi_until "" ->
    foldi_until "abcde" -> 0:a 1:b 2:c
    |}]

let%expect_test "fold" =
  let open Format in
  let test_fold s = begin
    printf "fold \"%s\" ->" s;
    let () = fold s ~init:() ~f:(fun _ cp -> printf " %s" (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold;

  [%expect{|
    fold "" ->
    fold "abcde" -> a b c d e
    |}]

let%expect_test "fold_right" =
  let open Format in
  let test_fold_right s = begin
    printf "fold_right \"%s\" ->" s;
    let () = fold_right s ~init:() ~f:(fun cp _ ->
      printf " %s" (of_codepoint cp)
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_fold_right;

  [%expect{|
    fold_right "" ->
    fold_right "abcde" -> e d c b a
    |}]

let%expect_test "foldi" =
  let open Format in
  let test_foldi s = begin
    printf "foldi \"%s\" ->" s;
    let () = foldi s ~init:() ~f:(fun i _ cp ->
      printf " %d:%s" (Uint.to_int i) (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_foldi;

  [%expect{|
    foldi "" ->
    foldi "abcde" -> 0:a 1:b 2:c 3:d 4:e
    |}]

let%expect_test "iter" =
  let open Format in
  let test_iter s = begin
    printf "iter \"%s\" ->" s;
    let () = iter s ~f:(fun cp -> printf " %s" (of_codepoint cp)) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iter;

  [%expect{|
    iter "" ->
    iter "abcde" -> a b c d e
    |}]

let%expect_test "iteri" =
  let open Format in
  let test_iteri s = begin
    printf "iteri \"%s\" ->" s;
    let () = iteri s ~f:(fun i cp ->
      printf " %d:%s" (Uint.to_int i) (of_codepoint cp)
    ) in
    printf "\n"
  end in
  let strs = [
    "";
    "abcde";
  ] in
  List.iter strs ~f:test_iteri;

  [%expect{|
    iteri "" ->
    iteri "abcde" -> 0:a 1:b 2:c 3:d 4:e
    |}]

let%expect_test "for_" =
  let open Format in
  let test_for s cp = begin
    let f codepoint = Codepoint.(codepoint = cp) in
    printf "for_any \"%s\" '%s' -> %B\n" s (of_codepoint cp) (for_any s ~f);
    printf "for_all \"%s\" '%s' -> %B\n" s (of_codepoint cp) (for_all s ~f);
    printf "mem \"%s\" '%s' -> %B\n" s (of_codepoint cp) (mem s cp);
  end in
  test_for "" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'a');
  test_for "abcde" Codepoint.(of_char 'b');
  test_for "abcde" Codepoint.(of_char 'f');
  test_for "fff" Codepoint.(of_char 'f');

  [%expect{|
    for_any "" 'a' -> false
    for_all "" 'a' -> true
    mem "" 'a' -> false
    for_any "abcde" 'a' -> true
    for_all "abcde" 'a' -> false
    mem "abcde" 'a' -> true
    for_any "abcde" 'b' -> true
    for_all "abcde" 'b' -> false
    mem "abcde" 'b' -> true
    for_any "abcde" 'f' -> false
    for_all "abcde" 'f' -> false
    mem "abcde" 'f' -> false
    for_any "fff" 'f' -> true
    for_all "fff" 'f' -> true
    mem "fff" 'f' -> true
    |}]

let%expect_test "find" =
  let open Format in
  let test_find s ~f = begin
    printf "find \"%s\" -> %s\n" s (match find s ~f with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
  end in
  test_find "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> true
    | _ -> false
  in
  test_find "abcde" ~f;
  test_find "ab de" ~f;

  [%expect{|
    find "" -> None
    find "abcde" -> 'c'
    find "ab de" -> None
    |}]

let%expect_test "find_map" =
  let open Format in
  let test_find_map s ~f = begin
    printf "find_map \"%s\" -> %s\n" s (match find_map s ~f with
      | None -> "None"
      | Some s -> s
    );
  end in
  test_find_map "" ~f:(fun _ -> not_reached ());
  let f = function
    | cp when Codepoint.(cp = of_char 'c') -> Some "'c'"
    | _ -> None
  in
  test_find_map "abcde" ~f;
  test_find_map "ab de" ~f;

  [%expect{|
    find_map "" -> None
    find_map "abcde" -> 'c'
    find_map "ab de" -> None
    |}]

let%expect_test "_elm" =
  let open Format in
  let test_elm s ~cmp = begin
    printf "min_elm \"%s\" -> %s\n" s (match min_elm s ~cmp with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
    printf "max_elm \"%s\" -> %s\n" s (match max_elm s ~cmp with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
  end in
  test_elm "" ~cmp:Codepoint.cmp;
  test_elm "baced" ~cmp:Codepoint.cmp;
  let cmp cp0 cp1 = begin
    match Codepoint.cmp cp0 cp1 with
    | Lt -> Cmp.Gt
    | Eq -> Cmp.Eq
    | Gt -> Cmp.Lt
  end in
  test_elm "" ~cmp;
  test_elm "baced" ~cmp;

  [%expect{|
    min_elm "" -> None
    max_elm "" -> None
    min_elm "baced" -> 'a'
    max_elm "baced" -> 'e'
    min_elm "" -> None
    max_elm "" -> None
    min_elm "baced" -> 'e'
    max_elm "baced" -> 'a'
    |}]

let%expect_test "init" =
  let open Format in
  let codepoints = Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ] in
  printf "init -> \"%s\"\n" (init (Array.length codepoints) ~f:(fun i ->
    Array.get codepoints i
  ));
  printf "of_codepoint -> \"%s\"\n" (of_codepoint (Codepoint.of_char 'a'));

  [%expect{|
    init -> "abcde"
    of_codepoint -> "a"
    |}]

let%expect_test "list" =
  let open Format in
  let test_list l = begin
    let s = of_list l in
    let l' = to_list s in
    let s' = of_list l' in
    printf "list: \"%s\" -> ... -> \"%s\"\n" s s';

    let s = of_list_rev l in
    let l' = to_list_rev s in
    let s' = of_list_rev l' in
    printf "list_rev: \"%s\" -> ... -> \"%s\"\n" s s';
  end in
  test_list [];
  test_list [
    (Codepoint.of_char 'a');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
  ];
  test_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ];

  [%expect{|
    list: "" -> ... -> ""
    list_rev: "" -> ... -> ""
    list: "a" -> ... -> "a"
    list_rev: "a" -> ... -> "a"
    list: "ab" -> ... -> "ab"
    list_rev: "ba" -> ... -> "ba"
    list: "abc" -> ... -> "abc"
    list_rev: "cba" -> ... -> "cba"
    list: "abcd" -> ... -> "abcd"
    list_rev: "dcba" -> ... -> "dcba"
    list: "abcde" -> ... -> "abcde"
    list_rev: "edcba" -> ... -> "edcba"
    |}]

let%expect_test "array" =
  let open Format in
  let test_array a = begin
    let s = of_array a in
    let a' = to_array s in
    let s' = of_array a' in
    printf "array: \"%s\" -> ... -> \"%s\"\n" s s';
  end in
  test_array [||];
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
  ]);
  test_array (Array.of_list [
    (Codepoint.of_char 'a');
    (Codepoint.of_char 'b');
    (Codepoint.of_char 'c');
    (Codepoint.of_char 'd');
    (Codepoint.of_char 'e');
  ]);

  [%expect{|
    array: "" -> ... -> ""
    array: "a" -> ... -> "a"
    array: "ab" -> ... -> "ab"
    array: "abc" -> ... -> "abc"
    array: "abcd" -> ... -> "abcd"
    array: "abcde" -> ... -> "abcde"
    |}]

let%expect_test "map" =
  let open Format in
  let s = "abcde" in
  printf "map: \"%s\" -> \"%s\"\n" s (map s ~f:(fun cp ->
    Codepoint.(cp - (kv 32))));
  printf "mapi: \"%s\" -> \"%s\"\n" s (mapi s ~f:(fun i cp ->
    match Uint.(bit_and i (kv 0x1)) with
    | bit when Uint.(bit = (kv 0)) -> cp
    | bit when Uint.(bit = (kv 1)) -> Codepoint.(cp - (kv 32))
    | _ -> not_reached ()
  ));
  let s = "a:b:cd:e" in
  printf "tr: \"%s\" -> \"%s\"\n" s (tr s ~target:Codepoint.(of_char ':')
      ~replacement:Codepoint.(of_char ' '));
  printf "filter: \"%s\" -> \"%s\"\n" s (filter s ~f:(fun codepoint ->
    Codepoint.(codepoint <> (of_char ':'))
  ));

  [%expect{|
    map: "abcde" -> "ABCDE"
    mapi: "abcde" -> "aBcDe"
    tr: "a:b:cd:e" -> "a b cd e"
    filter: "a:b:cd:e" -> "abcde"
    |}]

let%expect_test "rev" =
  let open Format in
  let test_rev s = begin
    printf "rev \"%s\" -> \"%s\"\n" s (rev s);
  end in
  test_rev "";
  test_rev "a";
  test_rev "ab";
  test_rev "abc";
  test_rev "abcd";

  [%expect{|
    rev "" -> ""
    rev "a" -> "a"
    rev "ab" -> "ba"
    rev "abc" -> "cba"
    rev "abcd" -> "dcba" |}]

let%expect_test "concat" =
  let open Format in
  printf "%s\n" (concat [""]);
  printf "%s\n" (concat [""; ""]);
  printf "%s\n" (concat [""; ""; ""]);

  printf "%s\n" (concat ~sep:":" [""]);
  printf "%s\n" (concat ~sep:":" [""; ""]);
  printf "%s\n" (concat ~sep:":" [""; ""; ""]);

  printf "%s\n" (concat ["a"]);
  printf "%s\n" (concat ["a"; ""]);
  printf "%s\n" (concat ["a"; "b"]);
  printf "%s\n" (concat ["a"; "b"; "c"]);

  printf "%s\n" (concat ~sep:":" ["a"; "b"; "c"]);
  printf "%s\n" (concat ~sep:".." ["a"; "b"; "c"]);
  printf "%s\n" (concat ~sep:":" ["ab"; "cd"; "ef"]);

  printf "%s\n" (concat ~sep:":" ["a"; ""; ""]);
  printf "%s\n" (concat ~sep:":" ["a"; "b"; ""]);
  printf "%s\n" (concat ~sep:":" ["a"; ""; "c"]);
  printf "%s\n" (concat ~sep:":" [""; "b"; "c"]);
  printf "%s\n" (concat ~sep:":" [""; ""; "c"]);

  printf "%s\n" (concat_rev ~sep:":" ["a"; "b"; "c"]);
  printf "%s\n" ("a" ^ "b" ^ "c");

  [%expect{|
    :
    ::
    a
    a
    ab
    abc
    a:b:c
    a..b..c
    ab:cd:ef
    a::
    a:b:
    a::c
    :b:c
    ::c
    c:b:a
    abc
    |}]

let%expect_test "concat_map" =
  let open Format in
  let s = "abcde\n" in
  printf "%s" s;
  printf "%s" (concat_map s ~f:(fun cp -> of_codepoint cp));
  printf "%s" (concat_map s ~f:(fun cp ->
    match cp with
    | cp when Codepoint.(cp = (kv 0x61)) -> "hello "
    | cp when Codepoint.(cp = (kv 0x64)) -> " there "
    | _ -> of_codepoint cp
  ));

  [%expect{|
    abcde
    abcde
    hello bc there e
    |}]

let%expect_test "escaped" =
  let open Format in

  let rec fn i = begin
    match i with
    | i when Uint.(i = (kv 0x80)) -> ()
    | _ -> begin
        printf "0x%02x -> \"%s\"\n"
          (Uint.to_int i) (escaped (of_codepoint Codepoint.(of_uint i)));
        fn (Uint.succ i)
      end
  end in
  fn (Uint.kv 0);

  [%expect{|
    0x00 -> "\0"
    0x01 -> "\x01"
    0x02 -> "\x02"
    0x03 -> "\x03"
    0x04 -> "\x04"
    0x05 -> "\x05"
    0x06 -> "\x06"
    0x07 -> "\a"
    0x08 -> "\b"
    0x09 -> "\t"
    0x0a -> "\n"
    0x0b -> "\v"
    0x0c -> "\f"
    0x0d -> "\r"
    0x0e -> "\x0e"
    0x0f -> "\x0f"
    0x10 -> "\x10"
    0x11 -> "\x11"
    0x12 -> "\x12"
    0x13 -> "\x13"
    0x14 -> "\x14"
    0x15 -> "\x15"
    0x16 -> "\x16"
    0x17 -> "\x17"
    0x18 -> "\x18"
    0x19 -> "\x19"
    0x1a -> "\x1a"
    0x1b -> "\x1b"
    0x1c -> "\x1c"
    0x1d -> "\x1d"
    0x1e -> "\x1e"
    0x1f -> "\x1f"
    0x20 -> " "
    0x21 -> "!"
    0x22 -> "\""
    0x23 -> "#"
    0x24 -> "$"
    0x25 -> "%"
    0x26 -> "&"
    0x27 -> "'"
    0x28 -> "("
    0x29 -> ")"
    0x2a -> "*"
    0x2b -> "+"
    0x2c -> ","
    0x2d -> "-"
    0x2e -> "."
    0x2f -> "/"
    0x30 -> "0"
    0x31 -> "1"
    0x32 -> "2"
    0x33 -> "3"
    0x34 -> "4"
    0x35 -> "5"
    0x36 -> "6"
    0x37 -> "7"
    0x38 -> "8"
    0x39 -> "9"
    0x3a -> ":"
    0x3b -> ";"
    0x3c -> "<"
    0x3d -> "="
    0x3e -> ">"
    0x3f -> "?"
    0x40 -> "@"
    0x41 -> "A"
    0x42 -> "B"
    0x43 -> "C"
    0x44 -> "D"
    0x45 -> "E"
    0x46 -> "F"
    0x47 -> "G"
    0x48 -> "H"
    0x49 -> "I"
    0x4a -> "J"
    0x4b -> "K"
    0x4c -> "L"
    0x4d -> "M"
    0x4e -> "N"
    0x4f -> "O"
    0x50 -> "P"
    0x51 -> "Q"
    0x52 -> "R"
    0x53 -> "S"
    0x54 -> "T"
    0x55 -> "U"
    0x56 -> "V"
    0x57 -> "W"
    0x58 -> "X"
    0x59 -> "Y"
    0x5a -> "Z"
    0x5b -> "["
    0x5c -> "\\"
    0x5d -> "]"
    0x5e -> "^"
    0x5f -> "_"
    0x60 -> "`"
    0x61 -> "a"
    0x62 -> "b"
    0x63 -> "c"
    0x64 -> "d"
    0x65 -> "e"
    0x66 -> "f"
    0x67 -> "g"
    0x68 -> "h"
    0x69 -> "i"
    0x6a -> "j"
    0x6b -> "k"
    0x6c -> "l"
    0x6d -> "m"
    0x6e -> "n"
    0x6f -> "o"
    0x70 -> "p"
    0x71 -> "q"
    0x72 -> "r"
    0x73 -> "s"
    0x74 -> "t"
    0x75 -> "u"
    0x76 -> "v"
    0x77 -> "w"
    0x78 -> "x"
    0x79 -> "y"
    0x7a -> "z"
    0x7b -> "{"
    0x7c -> "|"
    0x7d -> "}"
    0x7e -> "~"
    0x7f -> "\x7f"
    |}]

let%expect_test "find" =
  let open Format in
  let test_find s cp = begin
    printf "lfind \"%s\" '%s' -> %s\n" s (of_codepoint cp)
      (match lfind s cp with
       | None -> "<not found>"
       | Some cursor -> asprintf "%a" pp (Cursor.bindex cursor)
      );
    printf "contains \"%s\" '%s' -> %B\n" s (of_codepoint cp) (contains s cp);
    printf "rfind \"%s\" '%s' -> %s\n" s (of_codepoint cp)
      (match rfind s cp with
       | None -> "<not found>"
       | Some cursor -> asprintf "%a" pp (Cursor.bindex cursor)
      )
  end in
  test_find "" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'a');
  test_find "abcba" Codepoint.(of_char 'b');
  test_find "abcba" Codepoint.(of_char 'c');
  test_find "abcba" Codepoint.(of_char 'd');

  [%expect{|
    lfind "" 'b' -> <not found>
    contains "" 'b' -> false
    rfind "" 'b' -> <not found>
    lfind "abcba" 'a' -> 0
    contains "abcba" 'a' -> true
    rfind "abcba" 'a' -> 4
    lfind "abcba" 'b' -> 1
    contains "abcba" 'b' -> true
    rfind "abcba" 'b' -> 3
    lfind "abcba" 'c' -> 2
    contains "abcba" 'c' -> true
    rfind "abcba" 'c' -> 2
    lfind "abcba" 'd' -> <not found>
    contains "abcba" 'd' -> false
    rfind "abcba" 'd' -> <not found>
    |}]

let%expect_test "substr_find" =
  let open Format in
  let patterns = ["";
                  "a";
                  "aa";
                  "aba";

                  "ab";
                  "aab";
                  "aabab";
                  "aaabaabab";
                  "aaaabaaabaabab";
                  "aaaaaabaaaabaaabaabab";

                  "ab";
                  "abaab";
                  "abaabaaab";
                 ] in
  let s = concat patterns in
  List.iter patterns ~f:(fun pattern ->
    let p = Slice.Pattern.create (Slice.of_string pattern) in
    let () = Slice.Pattern.pretty_print p in

    printf "     in_:\"%s\"\n" s;

    let print_matches matches = begin
      match matches with
      | [] -> ()
      | matches -> begin
          let _ = List.fold matches ~init:None ~f:(fun prev cursor ->
            assert (match prev with
              | None -> true
              | Some c -> Uint.((Cursor.bindex c) < (Cursor.bindex cursor))
            );
            let offset = Uint.to_int (match prev with
              | None -> Uint.((Cursor.bindex cursor) + (kv 2))
              | Some c -> Uint.((Cursor.bindex cursor) - (Cursor.bindex c))
            ) in
            printf "%*s" offset "|";
            Some cursor
          ) in
          ()
        end
    end in

    printf "     all:";
    print_matches (substr_find_all s ~may_overlap:true ~pattern);
    printf "\n";

    printf "disjoint:";
    print_matches (substr_find_all s ~may_overlap:false ~pattern);
    printf "\n";

    printf "   first:";
    let () = match substr_find s ~pattern with
      | None -> ()
      | Some cursor ->
        printf " %*s" Uint.(to_int (succ (Cursor.bindex cursor))) "|";
    in
    printf "\n";
    printf "\n";
    ()
  );

  ();

  [%expect{|
     p=""
    pi=[]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all: |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
    disjoint: |||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||||
       first: |

     p="a"
    pi=[0]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all: |||| || || || | ||| || | |||| ||| || | |||||| |||| ||| || | | | || | || |||
    disjoint: |||| || || || | ||| || | |||| ||| || | |||||| |||| ||| || | | | || | || |||
       first: |

     p="aa"
    pi=[01]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all: |||  |  |  |    ||  |    |||  ||  |    |||||  |||  ||  |        |    |  ||
    disjoint: | |  |  |  |    |   |    | |  |   |    | | |  | |  |   |        |    |  |
       first: |

     p="aba"
    pi=[001]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:    |  |  |  | |   |  | |    |   |  | |      |    |   |  | | | |  | |  |
    disjoint:    |  |  |  |     |  |      |   |  |        |    |   |  |   |    |    |
       first:    |

     p="ab"
    pi=[00]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:    |  |  |  | |   |  | |    |   |  | |      |    |   |  | | | |  | |  |   |
    disjoint:    |  |  |  | |   |  | |    |   |  | |      |    |   |  | | | |  | |  |   |
       first:    |

     p="aab"
    pi=[010]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:   |  |  |  |     |  |      |   |  |        |    |   |  |        |    |   |
    disjoint:   |  |  |  |     |  |      |   |  |        |    |   |  |        |    |   |
       first:   |

     p="aabab"
    pi=[01010]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:            |        |             |                    |        |
    disjoint:            |        |             |                    |        |
       first:            |

     p="aaabaabab"
    pi=[012012010]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:                 |             |                    |
    disjoint:                 |             |                    |
       first:                 |

     p="aaaabaaabaabab"
    pi=[01230123012010]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:                          |                    |
    disjoint:                          |                    |
       first:                          |

     p="aaaaaabaaaabaaabaabab"
    pi=[012345012340123012010]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:                                        |
    disjoint:                                        |
       first:                                        |

     p="ab"
    pi=[00]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:    |  |  |  | |   |  | |    |   |  | |      |    |   |  | | | |  | |  |   |
    disjoint:    |  |  |  | |   |  | |    |   |  | |      |    |   |  | | | |  | |  |   |
       first:    |

     p="abaab"
    pi=[00112]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:    |  |  |        |             |                    |        |    |
    disjoint:    |     |        |             |                    |        |    |
       first:    |

     p="abaabaaab"
    pi=[001123412]
         in_:"aaaabaabaabaababaaabaababaaaabaaabaababaaaaaabaaaabaaabaababababaababaabaaab"
         all:                                                                    |
    disjoint:                                                                    |
       first:                                                                    |
    |}]

let%expect_test "substr_replace" =
  let open Format in
  let replacements = [
    (* pattern, with, in *)
    ("", "", "");
    ("", "", "abc");

    ("", "x", "abc");
    ("x", "y", "abc");

    ("", "x", "");
    ("a", "A", "abc");
    ("b", "B", "abc");
    ("c", "C", "abc");

    ("abc", "", "abc");
    ("abc", "A", "abc");
    ("abc", "AB", "abc");
    ("abc", "ABC", "abc");

    ("ab", "", "abc");
    ("ab", "A", "abc");
    ("ab", "AB", "abc");
    ("ab", "ABC", "abc");

    ("bc", "", "abc");
    ("bc", "A", "abc");
    ("bc", "AB", "abc");
    ("bc", "ABC", "abc");

    ("b", "B", "ababa");
    ("ab", "AB", "ababa");
    ("ba", "BA", "ababa");
  ] in
  List.iter replacements ~f:(fun (pattern, with_, in_) ->
    printf "s/%s/%s/ \"%s\" -> \"%s\"\n"
      pattern with_ in_ (substr_replace_first in_ ~pattern ~with_);
    printf "s/%s/%s/g \"%s\" -> \"%s\"\n"
      pattern with_ in_ (substr_replace_all in_ ~pattern ~with_);
    printf "\n"
  );

  [%expect{|
    s/// "" -> ""
    s///g "" -> ""

    s/// "abc" -> "abc"
    s///g "abc" -> "abc"

    s//x/ "abc" -> "xabc"
    s//x/g "abc" -> "xaxbxcx"

    s/x/y/ "abc" -> "abc"
    s/x/y/g "abc" -> "abc"

    s//x/ "" -> "x"
    s//x/g "" -> "x"

    s/a/A/ "abc" -> "Abc"
    s/a/A/g "abc" -> "Abc"

    s/b/B/ "abc" -> "aBc"
    s/b/B/g "abc" -> "aBc"

    s/c/C/ "abc" -> "abC"
    s/c/C/g "abc" -> "abC"

    s/abc// "abc" -> ""
    s/abc//g "abc" -> ""

    s/abc/A/ "abc" -> "A"
    s/abc/A/g "abc" -> "A"

    s/abc/AB/ "abc" -> "AB"
    s/abc/AB/g "abc" -> "AB"

    s/abc/ABC/ "abc" -> "ABC"
    s/abc/ABC/g "abc" -> "ABC"

    s/ab// "abc" -> "c"
    s/ab//g "abc" -> "c"

    s/ab/A/ "abc" -> "Ac"
    s/ab/A/g "abc" -> "Ac"

    s/ab/AB/ "abc" -> "ABc"
    s/ab/AB/g "abc" -> "ABc"

    s/ab/ABC/ "abc" -> "ABCc"
    s/ab/ABC/g "abc" -> "ABCc"

    s/bc// "abc" -> "a"
    s/bc//g "abc" -> "a"

    s/bc/A/ "abc" -> "aA"
    s/bc/A/g "abc" -> "aA"

    s/bc/AB/ "abc" -> "aAB"
    s/bc/AB/g "abc" -> "aAB"

    s/bc/ABC/ "abc" -> "aABC"
    s/bc/ABC/g "abc" -> "aABC"

    s/b/B/ "ababa" -> "aBaba"
    s/b/B/g "ababa" -> "aBaBa"

    s/ab/AB/ "ababa" -> "ABaba"
    s/ab/AB/g "ababa" -> "ABABa"

    s/ba/BA/ "ababa" -> "aBAba"
    s/ba/BA/g "ababa" -> "aBABA"
    |}]

let%expect_test "slice" =
  let open Format in
  let strs = [
    "";
    "<_>";
    "«»";
    "‡";
    "𐆗";
  ] in
  List.iter strs ~f:(fun s ->
    printf "\"%s\" |slice| -> \"%s\"\n"
      s (pare ~base:(Cursor.hd s) ~past:(Cursor.tl s));
    let () = match clength s with
      | clen when Uint.(clen = (kv 0)) -> ()
      | _ -> begin
          printf "\"%s\" .|slice| -> \"%s\"\n"
            s (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(tl s));
          printf "\"%s\" |slice|. -> \"%s\"\n"
            s (pare ~base:Cursor.(hd s) ~past:Cursor.(pred (tl s)))
        end
    in
    let () = match clength s with
      | clen when Uint.(clen = (kv 0)) -> ()
      | clen when Uint.(clen = (kv 1)) -> ()
      | _ ->
        printf "\"%s\" .|slice|. -> \"%s\"\n"
          s (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(pred (tl s)))
    in
    ()
  );

  [%expect{|
    "" |slice| -> ""
    "<_>" |slice| -> "<_>"
    "<_>" .|slice| -> "_>"
    "<_>" |slice|. -> "<_"
    "<_>" .|slice|. -> "_"
    "«»" |slice| -> "«»"
    "«»" .|slice| -> "»"
    "«»" |slice|. -> "«"
    "«»" .|slice|. -> ""
    "‡" |slice| -> "‡"
    "‡" .|slice| -> ""
    "‡" |slice|. -> ""
    "𐆗" |slice| -> "𐆗"
    "𐆗" .|slice| -> ""
    "𐆗" |slice|. -> ""
    |}]

let%expect_test "xfix" =
  let open Format in
  let strs = [
    "";
    "<_>";
    "«»";
  ] in
  List.iter strs ~f:(fun s ->
    for i = 0 to Uint.(to_int ((clength s) + (kv 1))) do
      let i = Uint.of_int i in
      printf "prefix \"%s\" %a -> \"%s\"\n" s pp i (prefix s i);
      printf "suffix \"%s\" %a -> \"%s\"\n" s pp i (suffix s i);
    done
  );

  [%expect{|
    prefix "" 0 -> ""
    suffix "" 0 -> ""
    prefix "" 1 -> ""
    suffix "" 1 -> ""
    prefix "<_>" 0 -> ""
    suffix "<_>" 0 -> ""
    prefix "<_>" 1 -> "<"
    suffix "<_>" 1 -> ">"
    prefix "<_>" 2 -> "<_"
    suffix "<_>" 2 -> "_>"
    prefix "<_>" 3 -> "<_>"
    suffix "<_>" 3 -> "<_>"
    prefix "<_>" 4 -> "<_>"
    suffix "<_>" 4 -> "<_>"
    prefix "«»" 0 -> ""
    suffix "«»" 0 -> ""
    prefix "«»" 1 -> "«"
    suffix "«»" 1 -> "»"
    prefix "«»" 2 -> "«»"
    suffix "«»" 2 -> "«»"
    prefix "«»" 3 -> "«»"
    suffix "«»" 3 -> "«»"
    |}]

let%expect_test "prefix" =
  let open Format in
  let test_prefix s ~prefix = begin
    printf "is_prefix \"%s\" ~prefix:\"%s\" -> %B\n" s prefix
      (is_prefix s ~prefix);
    printf "chop_prefix \"%s\" ~prefix:\"%s\" -> %s\n" s prefix
      (match chop_prefix s ~prefix with
       | None -> "None"
       | Some s' -> "\"" ^ s' ^ "\""
      )
  end in

  test_prefix "abc" ~prefix:"";
  test_prefix "abc" ~prefix:"a";
  test_prefix "abc" ~prefix:"ab";
  test_prefix "abc" ~prefix:"abc";

  test_prefix "abc" ~prefix:"d";
  test_prefix "abc" ~prefix:"ad";
  test_prefix "abc" ~prefix:"abd";
  test_prefix "abc" ~prefix:"abcd";

  [%expect{|
    is_prefix "abc" ~prefix:"" -> true
    chop_prefix "abc" ~prefix:"" -> "abc"
    is_prefix "abc" ~prefix:"a" -> true
    chop_prefix "abc" ~prefix:"a" -> "bc"
    is_prefix "abc" ~prefix:"ab" -> true
    chop_prefix "abc" ~prefix:"ab" -> "c"
    is_prefix "abc" ~prefix:"abc" -> true
    chop_prefix "abc" ~prefix:"abc" -> ""
    is_prefix "abc" ~prefix:"d" -> false
    chop_prefix "abc" ~prefix:"d" -> None
    is_prefix "abc" ~prefix:"ad" -> false
    chop_prefix "abc" ~prefix:"ad" -> None
    is_prefix "abc" ~prefix:"abd" -> false
    chop_prefix "abc" ~prefix:"abd" -> None
    is_prefix "abc" ~prefix:"abcd" -> false
    chop_prefix "abc" ~prefix:"abcd" -> None |}]

let%expect_test "suffix" =
  let open Format in
  let test_suffix s ~suffix = begin
    printf "is_suffix \"%s\" ~suffix:\"%s\" -> %B\n" s suffix
      (is_suffix s ~suffix);
    printf "chop_suffix \"%s\" ~suffix:\"%s\" -> %s\n" s suffix
      (match chop_suffix s ~suffix with
       | None -> "None"
       | Some s' -> "\"" ^ s' ^ "\""
      )
  end in

  test_suffix "abc" ~suffix:"";
  test_suffix "abc" ~suffix:"c";
  test_suffix "abc" ~suffix:"bc";
  test_suffix "abc" ~suffix:"abc";

  test_suffix "abc" ~suffix:"d";
  test_suffix "abc" ~suffix:"dc";
  test_suffix "abc" ~suffix:"dab";
  test_suffix "abc" ~suffix:"dabc";

  [%expect{|
    is_suffix "abc" ~suffix:"" -> true
    chop_suffix "abc" ~suffix:"" -> "abc"
    is_suffix "abc" ~suffix:"c" -> true
    chop_suffix "abc" ~suffix:"c" -> "ab"
    is_suffix "abc" ~suffix:"bc" -> true
    chop_suffix "abc" ~suffix:"bc" -> "a"
    is_suffix "abc" ~suffix:"abc" -> true
    chop_suffix "abc" ~suffix:"abc" -> ""
    is_suffix "abc" ~suffix:"d" -> false
    chop_suffix "abc" ~suffix:"d" -> None
    is_suffix "abc" ~suffix:"dc" -> false
    chop_suffix "abc" ~suffix:"dc" -> None
    is_suffix "abc" ~suffix:"dab" -> false
    chop_suffix "abc" ~suffix:"dab" -> None
    is_suffix "abc" ~suffix:"dabc" -> false
    chop_suffix "abc" ~suffix:"dabc" -> None |}]

let%expect_test "strip" =
  let open Format in
  let test_strip ?drop s = begin
    printf "lstrip \"%s\" -> \"%s\"\n" s (lstrip ?drop s);
    printf "rstrip \"%s\" -> \"%s\"\n" s (rstrip ?drop s);
    printf "strip \"%s\" -> \"%s\"\n" s (strip ?drop s);
  end in
  test_strip "  a b c  ";
  test_strip ~drop:(fun codepoint ->
    Codepoint.(codepoint = (kv 0x5f) (* '_' *))
  ) "_ a_b_c _";

  [%expect{|
    lstrip "  a b c  " -> "a b c  "
    rstrip "  a b c  " -> "  a b c"
    strip "  a b c  " -> "a b c"
    lstrip "_ a_b_c _" -> " a_b_c _"
    rstrip "_ a_b_c _" -> "_ a_b_c "
    strip "_ a_b_c _" -> " a_b_c "
    |}]

let%expect_test "split" =
  let open Format in
  let test_split s f cp = begin
    printf "split \"%s\" -> [" s;
    List.iteri (split s ~f) ~f:(fun i substr ->
      if Uint.(i > (kv 0)) then printf "; ";
      printf "\"%s\"" substr
    );
    printf "]\n";

    printf "split_rev \"%s\" -> [" s;
    List.iteri (split_rev s ~f)~f:(fun i substr ->
      if Uint.(i > (kv 0)) then printf "; ";
      printf "\"%s\"" substr
    );
    printf "]\n";

    let s1, s2 = lsplit2_hlt s ~on:cp in
    printf "lsplit2_hlt \"%s\" -> (\"%s\", \"%s\")\n" s s1 s2;

    let s1, s2 = rsplit2_hlt s ~on:cp in
    printf "rsplit2_hlt \"%s\" -> (\"%s\", \"%s\")\n" s s1 s2;
  end in
  test_split ";a::bc;de;" (fun cp -> Codepoint.(cp = (kv (Char.code ':'))))
    (Codepoint.kv 0x3a);
  test_split ":a::bc;de:" (fun cp -> Codepoint.(cp = (kv (Char.code ':'))))
    (Codepoint.kv 0x3b);
  test_split ":a::bc;de;" (fun cp ->
    match Codepoint.to_int cp with
    | 0x3a (* : *)
    | 0x3b (* ; *) -> true
    | _ -> false
  ) (Codepoint.kv 0x3b);

  [%expect{|
    split ";a::bc;de;" -> [";a"; ""; "bc;de;"]
    split_rev ";a::bc;de;" -> ["bc;de;"; ""; ";a"]
    lsplit2_hlt ";a::bc;de;" -> (";a", ":bc;de;")
    rsplit2_hlt ";a::bc;de;" -> (";a:", "bc;de;")
    split ":a::bc;de:" -> [""; "a"; ""; "bc;de"; ""]
    split_rev ":a::bc;de:" -> [""; "bc;de"; ""; "a"; ""]
    lsplit2_hlt ":a::bc;de:" -> (":a::bc", "de:")
    rsplit2_hlt ":a::bc;de:" -> (":a::bc", "de:")
    split ":a::bc;de;" -> [""; "a"; ""; "bc"; "de"; ""]
    split_rev ":a::bc;de;" -> [""; "de"; "bc"; ""; "a"; ""]
    lsplit2_hlt ":a::bc;de;" -> (":a::bc", "de;")
    rsplit2_hlt ":a::bc;de;" -> (":a::bc;de", "")
    |}]

let%expect_test "split_lines" =
  let open Format in
  let test_split_lines s = begin
    printf "split_lines %S -> [" s;
    List.iteri (split_lines s)~f:(fun i substr ->
      if Uint.(i > (kv 0)) then printf "; ";
      printf "%S" substr
    );
    printf "]\n";

    printf "split_lines_rev %S -> [" s;
    List.iteri (split_lines_rev s)~f:(fun i substr ->
      if Uint.(i > (kv 0)) then printf "; ";
      printf "%S" substr
    );
    printf "]\n";
  end in
  test_split_lines "ab";

  test_split_lines "\nab";
  test_split_lines "a\nb";
  test_split_lines "ab\n";
  test_split_lines "\na\nb\n";

  test_split_lines "\r\nab";
  test_split_lines "a\r\nb";
  test_split_lines "ab\r\n";
  test_split_lines "\r\na\r\nb\r\n";

  test_split_lines "a\r\r\nb";

  test_split_lines "a\n\nb";
  test_split_lines "a\r\n\r\nb";

  test_split_lines "a\n\r\nb";
  test_split_lines "a\r\n\nb";

  test_split_lines "a\n\n\nb";

  [%expect{|
    split_lines "ab" -> ["ab"]
    split_lines_rev "ab" -> ["ab"]
    split_lines "\nab" -> [""; "ab"]
    split_lines_rev "\nab" -> ["ab"; ""]
    split_lines "a\nb" -> ["a"; "b"]
    split_lines_rev "a\nb" -> ["b"; "a"]
    split_lines "ab\n" -> ["ab"; ""]
    split_lines_rev "ab\n" -> [""; "ab"]
    split_lines "\na\nb\n" -> [""; "a"; "b"; ""]
    split_lines_rev "\na\nb\n" -> [""; "b"; "a"; ""]
    split_lines "\r\nab" -> [""; "ab"]
    split_lines_rev "\r\nab" -> ["ab"; ""]
    split_lines "a\r\nb" -> ["a"; "b"]
    split_lines_rev "a\r\nb" -> ["b"; "a"]
    split_lines "ab\r\n" -> ["ab"; ""]
    split_lines_rev "ab\r\n" -> [""; "ab"]
    split_lines "\r\na\r\nb\r\n" -> [""; "a"; "b"; ""]
    split_lines_rev "\r\na\r\nb\r\n" -> [""; "b"; "a"; ""]
    split_lines "a\r\r\nb" -> ["a\r"; "b"]
    split_lines_rev "a\r\r\nb" -> ["b"; "a\r"]
    split_lines "a\n\nb" -> ["a"; ""; "b"]
    split_lines_rev "a\n\nb" -> ["b"; ""; "a"]
    split_lines "a\r\n\r\nb" -> ["a"; ""; "b"]
    split_lines_rev "a\r\n\r\nb" -> ["b"; ""; "a"]
    split_lines "a\n\r\nb" -> ["a"; ""; "b"]
    split_lines_rev "a\n\r\nb" -> ["b"; ""; "a"]
    split_lines "a\r\n\nb" -> ["a"; ""; "b"]
    split_lines_rev "a\r\n\nb" -> ["b"; ""; "a"]
    split_lines "a\n\n\nb" -> ["a"; ""; ""; "b"]
    split_lines_rev "a\n\n\nb" -> ["b"; ""; ""; "a"]
    |}]
