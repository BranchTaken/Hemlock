open Rudiments0

module T = struct
  type t = string

  let blength t =
    Stdlib.String.length t

  let hash_fold t state =
    state
    |> Hash.State.Gen.init
    |> Hash.State.Gen.fold_u8 (Caml.String.length t) ~f:(fun i ->
      Caml.Char.code (Caml.String.get t i)
    )
    |> Hash.State.Gen.fini
    |> Uns.hash_fold (blength t)

  let cmp t0 t1 =
    let rel = Sint.of_int (compare t0 t1) in
    if Sint.(rel < (kv 0)) then
      Cmp.Lt
    else if Sint.(rel = (kv 0)) then
      Cmp.Eq
    else
      Cmp.Gt

  module CodepointSeq = struct
    module T = struct
      type outer = t
      type t = {
        string: outer;
        bindex: uns;
      }

      let init t =
        {string=t; bindex=0}

      let length t =
        (blength t.string) - t.bindex

      let next t =
        match length t with
        | 0 -> None
        | _ -> begin
            let b = Byte.of_char (Stdlib.String.get t.string t.bindex) in
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

let is_empty t =
  Uns.((blength t) = 0)

let get bindex t =
  Byte.of_sint_hlt
    (sint_of_int (Stdlib.Char.code (Stdlib.String.get t bindex)))

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
    {string; bindex=0}

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

  let at ~bindex string =
    let tl_index = blength string in
    if Uns.(bindex = tl_index) then
      {string; bindex}
    else begin
      let b:byte = get bindex string in
      if Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) then
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
        match Byte.((bit_and b (kv 0b11_000000)) <> (kv 0b10_000000)) with
        | true -> {string; bindex}
        | false -> fn (Uns.pred bindex)
      end in
      fn bindex
    end

  let seek coffset t =
    let rec left coffset t = begin
      match Sint.(coffset = (kv 0)) with
      | true -> t
      | false -> begin
          let t' = near t.string ~bindex:(Uns.pred t.bindex) in
          let coffset' = Sint.(pred coffset) in
          left coffset' t'
        end
    end in
    let rec right coffset t = begin
      match Sint.(coffset = (kv 0)) with
      | true -> t
      | false -> begin
          let b = get t.bindex t.string in
          let nbytes =
            if Byte.(b <= (kv 0b0111_1111)) then 1
            else Byte.(bit_clz (bit_not b))
          in
          let t' = {t with bindex=t.bindex + nbytes} in
          let coffset' = Sint.(pred coffset) in
          right coffset' t'
        end
    end in
    if Sint.(coffset < (kv 0)) then
      left Sint.(neg coffset) t
    else
      right coffset t

  let pred t =
    seek (Sint.kv (-1)) t

  let succ t =
    seek (Sint.kv 1) t

  let prev t =
    let bindex = Uns.pred t.bindex in
    let b = Byte.to_uns (get bindex t.string) in
    if Uns.(b <= 0b0111_1111) then (Codepoint.of_uns b), {t with bindex}
    else begin
      let rec fn string bindex u nbits = begin
        let b = Byte.to_uns (get bindex string) in
        match Uns.((bit_and b 0b11_000000) <> 0b10_000000) with
        | true -> begin
            let mask = bit_usr ~shift:(nbits/6) 0x3f in
            let bits = bit_and b mask in
            let u' = bit_or u (bit_sl ~shift:nbits bits) in
            (Codepoint.of_uns u'), {string; bindex}
          end
        | false -> begin
            let bindex' = Uns.pred bindex in
            let bits = bit_and b 0b00_111111 in
            let u' = bit_or (bit_sl ~shift:nbits bits) u in
            let nbits' = nbits + 6 in
            fn string bindex' u' nbits'
          end
      end in
      let bindex' = Uns.pred bindex in
      let u = bit_and b 0b00_111111 in
      let nbits = 6 in
      fn t.string bindex' u nbits
    end

  let next t =
    let b = Byte.to_uns (get t.bindex t.string) in
    let bindex' = Uns.succ t.bindex in
    if Uns.(b <= 0b0111_1111) then (Codepoint.of_uns b), {t with bindex=bindex'}
    else begin
      let rec fn u string bindex rem_bytes = begin
        match rem_bytes with
        | 0 -> (Codepoint.of_uns u), {string; bindex}
        | _ -> begin
            let b = Byte.to_uns (get bindex string) in
            let bits = bit_and b 0b00_111111 in
            let u' = bit_or (bit_sl ~shift:6 u) bits in
            let bindex' = Uns.succ bindex in
            fn u' string bindex' (Uns.pred rem_bytes)
          end
      end in
      let nbytes = Byte.(bit_clz (bit_not (of_uns b))) in
      let b0_nbits = 7 - nbytes in
      let b0_mask = (bit_sl ~shift:b0_nbits 1) - 1 in
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
  fn (Cursor.hd t) 0

let length = clength

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
    {cursor=(Cursor.hd string); cindex=0}

  let tl string =
    {cursor=(Cursor.tl string); cindex=(clength string)}

  let string t =
    Cursor.string t.cursor

  let container = string

  let index _ =
    not_reached ()

  let bindex t =
    Cursor.bindex t.cursor

  let seek coffset t =
    (* coffset may be negative, but it's okay to convert blindly to uns
     * because 2s complement addition does the right thing. *)
    {cursor=(Cursor.seek coffset t.cursor);
      cindex=Uns.(t.cindex + (of_sint coffset))}

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
    {cursor=(Cursor.seek (Uns.to_sint cindex) (Cursor.hd s));
      cindex}
end
type cursori = Cursori.t

module SlicePre = struct
  include Slice.MakeMono(Cursor)
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
        | 0 -> ""
        | _ -> begin
            let tmut = ref t in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init len (fun _ ->
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
                  Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
                end
            ) in
            assert (Uns.((List.length !rem_bytes) = 0));
            s
          end
    end

    module MakeRev (T : SeqIntf.IMonoDef with type elm := codepoint) :
      S with type t := T.t = struct
      let to_string t =
        let len = T.length t in
        match len with
        | 0 -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the
             * codepoints in order to manually reverse them. *)
            let rec fn t cps = begin
              match T.length t with
              | 0 -> cps
              | _ -> begin
                  let cp, t' = T.next t in
                  assert (Uns.((Codepoint.Utf8.length_of_codepoint cp) +
                      (T.length t') = (T.length t)));
                  let cps' = cp :: cps in
                  fn t' cps'
                end
            end in
            let cps = ref (fn t []) in
            let rem_bytes = ref [] in
            let s = Stdlib.String.init len (fun _ ->
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
                  Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
                end
              | b :: tl -> begin
                  rem_bytes := tl;
                  Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
                end
            ) in
            assert (Uns.((List.length !rem_bytes) = 0));
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
        | 0 -> ""
        | _ -> begin
            let tmut = ref t in
            let slice_str = ref "" in
            let slice_base = ref 0 in
            let slice_ind = ref 0 in
            let slice_len = ref 0 in
            let s = Stdlib.String.init len (fun _ ->
              let rec fn () = begin
                match Uns.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, t' = T.next !tmut in
                    let slice_base' = Cursor.bindex (SlicePre.base slice) in
                    let slice_len' =
                      (Cursor.bindex (SlicePre.past slice)) - slice_base' in
                    assert (Uns.(slice_len' + (T.length t') =
                      (T.length !tmut)));
                    tmut := t';

                    slice_str := Cursor.string (SlicePre.base slice);
                    slice_base := slice_base';
                    slice_ind := 0;
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get (!slice_base + !slice_ind) !slice_str in
                    slice_ind := Uns.succ !slice_ind;
                    Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
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
        | 0 -> ""
        | _ -> begin
            (* Stdlib.String.init_rev doesn't exist, so accumulate the strings
             * in order to manually reverse them. *)
            let rec fn t slices = begin
              match T.length t with
              | 0 -> slices
              | _ -> begin
                  let slice, t' = T.next t in
                  let slices' = slice :: slices in
                  fn t' slices'
                end
            end in
            let slices = ref (fn t []) in

            let slice_str = ref "" in
            let slice_base = ref 0 in
            let slice_ind = ref 0 in
            let slice_len = ref 0 in
            let s = Stdlib.String.init len (fun _ ->
              let rec fn () = begin
                match Uns.(!slice_ind = !slice_len) with
                | true -> begin
                    let slice, slices' = match !slices with
                      | slice :: slices' -> slice, slices'
                      | [] -> not_reached ()
                    in
                    let slice_base' = Cursor.bindex (SlicePre.base slice) in
                    let slice_len' =
                      (Cursor.bindex (SlicePre.past slice)) - slice_base' in
                    slices := slices';
                    slice_str := Cursor.string (SlicePre.base slice);
                    slice_base := slice_base';
                    slice_ind := 0;
                    slice_len := slice_len';
                    fn ()
                  end
                | false -> begin
                    let b = get (!slice_base + !slice_ind) !slice_str in
                    slice_ind := Uns.succ !slice_ind;
                    Stdlib.Char.chr (int_of_sint (Byte.to_sint b))
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
          let slice = SlicePre.of_container str in
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
          let slice = SlicePre.of_container str in
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
          (* Consider contained slices to come after their containers in the
           * total order. *)
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

  let of_string s =
    SlicePre.of_container s

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

  let is_empty t =
    Uns.((blength t) = 0)

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
            fn (base t) 0
          end
      end

  let length = clength

  let get bindex t =
    match Uns.(bindex > (blength t)) with
    | true -> halt "Out of bounds"
    | false -> begin
        Byte.of_uns_hlt (Stdlib.Char.code (Stdlib.String.unsafe_get (string t)
          ((Cursor.bindex (base t)) + bindex)))
      end

  module StringOfIndexed = struct
    module T = struct
      type t = {
        f: uns -> codepoint;
        blength: uns;
        cindex: uns;
      }

      let init ~f blength =
        {f; blength; cindex=0}

      let length t =
        t.blength

      let next t =
        let codepoint = t.f t.cindex in
        let cp_nbytes = Codepoint.Utf8.length_of_codepoint codepoint in
        let blength' = t.blength - cp_nbytes in
        let t' = {t with cindex=(Uns.succ t.cindex);
             blength=blength'} in
        codepoint, t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_seq clength ~seq ~f =
    let rec fn ~seq cindex nbytes = begin
      match Uns.(cindex = clength) with
      | true -> nbytes
      | false -> begin
          let codepoint, seq' = f seq in
          let cp_nbytes = Codepoint.Utf8.length_of_codepoint codepoint in
          let nbytes' = nbytes + cp_nbytes in
          fn ~seq:seq' (Uns.succ cindex) nbytes'
        end
    end in
    fn ~seq 0 0

  let init ?blength clength ~f =
    let blength = match blength with
      | None -> blength_of_seq clength ~seq:0
          ~f:(fun seq ->
            (f seq), (Uns.succ seq)
          )
      | Some blength -> blength
    in
    of_string StringOfIndexed.(to_string (init ~f blength))

  let of_codepoint codepoint =
    init 1 ~f:(fun _ -> codepoint)

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
    of_string StringOfList.(to_string (init codepoints blength))

  let of_list_rev ?blength ?clength codepoints_rev =
    let clength = match clength with
      | None -> List.length codepoints_rev
      | Some clength -> clength
    in
    let blength = match blength with
      | None -> blength_of_list clength codepoints_rev
      | Some blength -> blength
    in
    of_string StringOfListRev.(to_string (init codepoints_rev blength))

  let of_array ?blength codepoints =
    init ?blength (Array.length codepoints) ~f:(fun i ->
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
  include ContainerCommon.MakeMonoFold(U)
  include ContainerCommon.MakeMonoMem(U)
  include ContainerArray.MakeMonoArray(U)

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
          cindex=0;
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
        let t' = {t with cursor=cursor';
                         cindex=cindex';
                         blength=blength'} in
        codepoint', t'
    end
    include T
    include Seq.Codepoint.Make(T)
  end

  let blength_of_map t ~f =
    foldi t ~init:(0, false) ~f:(fun i (blength, modified) codepoint ->
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
              | 0 -> fn {t with source=Str}
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
    let _, blength = List.fold slices ~init:(0, 0)
      ~f:(fun (i, len) slice ->
        let i' = Uns.succ i in
        let sep_len = match i with
          | 0 -> 0
          | _ -> blength sep
        in
        let len' = sep_len + len + (blength slice) in
        i', len'
      ) in
    match (length sep), (List.length slices) with
    | _, 0 -> of_string ""
    | 0, 1 -> List.hd slices
    | _ -> of_string (StringConcat.to_string
        (StringConcat.init sep slices blength))

  let concat_rev ?(sep=(of_string "")) slices_rev =
    let slices, blength = List.fold slices_rev ~init:([], 0)
      ~f:(fun (slices, len) slice ->
        let slices' = slice :: slices in
        let sep_len = match slices with
          | [] -> 0
          | _ -> blength sep
        in
        let len' = sep_len + len + (blength slice) in
        slices', len'
      ) in
    match (length sep), (List.length slices) with
    | _, 0 -> of_string ""
    | 0, 1 -> List.hd slices
    | _ -> of_string (StringConcat.to_string
        (StringConcat.init sep slices blength))

  let concat_map ?sep ~f t =
    (* Iterate in reverse order to generate a list of slices that can then be
     * passed to concat. *)
    let modified, slices = fold_right t ~init:(false, [])
      ~f:(fun cp (modified, slices) ->
        let slice = f cp in
        let modified' = modified
                        || Uns.((blength slice)
                          <> (Codepoint.Utf8.length_of_codepoint cp))
                        || Codepoint.(Cursor.(rget (base slice)) <> cp) in
        let slices' = slice :: slices in
        modified', slices'
      ) in
    match modified, sep with
    | false, None -> t
    | _ -> concat ?sep slices

  let escaped t =
    concat_map t ~f:(fun cp ->
      of_string Codepoint.Utf8.(escape (of_codepoint cp)))

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
      (* Auxilliary data structure that enables Knuth-Morris-Pratt (KMP) pattern
       * matching. The cursors correspond to the codepoints of p. Each cursor
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
        | 0 -> [||]
        | _ -> begin
            let k = Cursori.hd s in
            let q = Cursori.succ k in
            let pi = Array.init (Cursori.cindex m) ~f:(fun _ -> k) in
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
            let cursor = (Cursor.at (string in_)
              ~bindex:((Cursor.bindex (base in_))
                + (Cursor.bindex i) - (Cursori.bindex m))) in
            let matches' = cursor :: matches in
            let nmatches' = Uns.succ nmatches in
            match may_overlap, Uns.((Cursori.bindex m) = 0),
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
                  Array.get (Uns.pred (Cursori.cindex q)) t.pi in
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
                      (Uns.is_positive (Cursori.cindex q)) with
                | true -> begin
                    let q' =
                      Array.get (Uns.pred (Cursori.cindex q)) t.pi in
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
      fn ~q ~i [] 0

    let find ~in_ t =
      let cursors =
        find_impl ~max_matches:1 ~may_overlap:false ~in_ t in
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
              let in_cursor = (Cursor.at cursor.string ~bindex:(
                cursor.bindex + (string_blength pattern))) in
              let slice = with_ in
              in_cursor, slice, at', With
            | cursor :: _ -> begin
                let slice = of_cursors ~base:(base in_) ~past:cursor in
                past in_, slice, at, In
              end
          in
          let ncursors = List.length at in
          let blength = ((blength in_) +
              (ncursors * ((blength with_) - (string_blength pattern)))) in
          {in_; pattern; with_; at=at'; in_cursor; slice; source; blength}

        let length t =
          t.blength

        let next t =
          let blength' = (t.blength - Cursor.((bindex (past t.slice)) -
              bindex (base t.slice))) in
          let t' = match t.source with
            | In -> begin
                let in_cursor', slice', at' = match t.at with
                  | [] -> begin
                      let slice' = of_cursors ~base:(past t.in_)
                        ~past:(past t.in_) in
                      past t.in_, slice', []
                    end
                  | cursor :: at' -> begin
                      let in_cursor' = Cursor.seek
                          (Uns.to_sint (string_clength t.pattern)) cursor in
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
                    of_cursors ~base:t.in_cursor ~past:(past t.in_)
                  | cursor :: _ ->
                    of_cursors ~base:t.in_cursor ~past:cursor
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

    let replace_first ~in_ ~with_ t =
      match find t ~in_ with
      | None -> in_
      | Some cursor -> of_string (StringPatternReplace.(to_string
          (init ~pattern:t.p ~in_ ~with_ ~at:[cursor])))

    let replace_all ~in_ ~with_ t =
      match find_all t ~may_overlap:false ~in_ with
      | [] -> in_
      | cursors -> of_string (StringPatternReplace.(to_string
          (init ~pattern:t.p ~in_ ~with_ ~at:cursors)))
  end

  let prefix_tl ~prefix t =
    let rec fn t_cursor prefix_cursor = begin
      let end_of_t = Cursor.(t_cursor = past t) in
      let end_of_prefix = Cursor.(prefix_cursor = past prefix) in
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
          match Codepoint.((Cursor.lget t_cursor)
            = (Cursor.lget suffix_cursor)) with
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
      | false -> Cursor.(seek (Uns.to_sint n) base)
    in
    of_cursors ~base ~past

  let suffix n t =
    let past = past t in
    let base = match Uns.((clength t) < n) with
      | true -> base t
      | false -> Cursor.(seek Sint.(neg (Uns.to_sint n)) past)
    in
    of_cursors ~base ~past

  let chop_prefix ~prefix t =
    match prefix_tl ~prefix t with
    | None -> None
    | Some base -> Some (of_cursors ~base ~past:(past t))

  let chop_prefix_hlt ~prefix t =
    match chop_prefix ~prefix t with
    | None -> halt "Not a prefix"
    | Some slice -> slice

  let chop_suffix ~suffix t =
    match suffix_hd ~suffix t with
    | None -> None
    | Some past -> Some (of_cursors ~base:(base t) ~past)

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

  let split_fold_until ~init ~on ~f t =
    let rec fn base past accum = begin
      match Cursor.(past = SlicePre.past t) with
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
    fn (base t) (base t) init

  let split_fold ~init ~on ~f t =
    split_fold_until ~init ~on ~f:(fun accum slice -> (f accum slice), false) t

  let split_fold_right_until ~init ~on ~f t =
    let rec fn base past accum = begin
      match Cursor.(base = SlicePre.base t) with
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
    fn (past t) (past t) init

  let split_fold_right ~init ~on ~f t =
    split_fold_right_until ~init ~on
      ~f:(fun slice accum -> (f slice accum), false) t

  let lines_fold ~init ~f t =
    let rec fn base past cr_seen accum = begin
      match Cursor.(past = SlicePre.past t) with
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
    fn (base t) (base t) false init

  let lines_fold_right ~init ~f t =
    let rec fn base past nl_seen accum = begin
      match Cursor.(base = SlicePre.base t) with
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
    fn (past t) (past t) false init

  let lsplit2 ~on t =
    split_fold_until ~init:None ~on:(fun codepoint ->
      Codepoint.(codepoint = on)
    ) ~f:(fun _ slice ->
      let base = Cursor.succ (past slice) in
      let past = past t in
      let slice2 = of_cursors ~base ~past in
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
      let slice0 = of_cursors ~base ~past in
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
include ContainerCommon.MakeMonoFold(U)
include ContainerCommon.MakeMonoMem(U)
include ContainerArray.MakeMonoArray(U)

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
  Slice.lfind codepoint (Slice.of_cursors ~base ~past)

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
  Slice.rfind codepoint (Slice.of_cursors ~base ~past)

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
  let in_ = Slice.of_cursors ~base ~past in
  Slice.Pattern.find ~in_ p

let substr_find_hlt ?base ~pattern t =
  let p = Slice.Pattern.create (Slice.of_string pattern) in
  let base = match base with
    | None -> Cursor.hd t
    | Some cursor -> cursor
  in
  let past = Cursor.tl t in
  let in_ = Slice.of_cursors ~base ~past in
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
  let in_ = Slice.of_cursors ~base ~past in
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

let pare ~base ~past =
  match Cursor.(base = (hd (container base)))
        && Cursor.(past = (tl (container past))) with
  | true -> Cursor.container base
  | false -> Slice.to_string (Slice.of_cursors ~base ~past)

let prefix n t =
  Slice.(to_string (prefix n (of_string t)))

let suffix n t =
  Slice.(to_string (suffix n (of_string t)))

let chop_prefix ~prefix t =
  let slice_opt = Slice.chop_prefix ~prefix:(Slice.of_string prefix)
    (Slice.of_string t) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_prefix_hlt ~prefix t =
  Slice.to_string (Slice.chop_prefix_hlt ~prefix:(Slice.of_string prefix)
    (Slice.of_string t))

let chop_suffix ~suffix t =
  let slice_opt = Slice.chop_suffix ~suffix:(Slice.of_string suffix)
    (Slice.of_string t) in
  match slice_opt with
  | None -> None
  | Some slice -> Some (Slice.to_string slice)

let chop_suffix_hlt ~suffix t =
  Slice.to_string (Slice.chop_suffix_hlt ~suffix:(Slice.of_string suffix)
    (Slice.of_string t))

let lstrip ?drop t =
  Slice.(to_string (lstrip ?drop (of_string t)))

let rstrip ?drop t =
  Slice.(to_string (rstrip ?drop (of_string t)))

let strip ?drop t =
  Slice.(to_string (strip ?drop (of_string t)))

let split ~f t =
  Slice.split_fold_right ~init:[] ~on:f
    ~f:(fun slice strings -> (Slice.to_string slice) :: strings)
    (Slice.of_string t)

let split_rev ~f t =
  Slice.split_fold ~init:[] ~on:f
    ~f:(fun strings slice -> (Slice.to_string slice) :: strings)
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
  | Some (slice, slice2) ->
    Some ((Slice.to_string slice), (Slice.to_string slice2))

let lsplit2_hlt ~on t =
  let slice, slice2 = Slice.lsplit2_hlt ~on (Slice.of_string t) in
  (Slice.to_string slice), (Slice.to_string slice2)

let rsplit2 ~on t =
  match Slice.rsplit2 ~on (Slice.of_string t) with
  | None -> None
  | Some (slice, slice2) ->
    Some ((Slice.to_string slice), (Slice.to_string slice2))

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

(******************************************************************************)
(* Begin tests. *)

let%expect_test "hash_fold" =
  let open Format in
  printf "@[<h>";
  let rec test strs = begin
    match strs with
    | [] -> ()
    | s :: strs' -> begin
        printf "hash_fold %a -> %a\n"
          pp s Hash.pp (Hash.t_of_state (hash_fold s Hash.State.empty));
        test strs'
      end
  end in
  (* These test inputs were manually verified against the reference MurmurHash3
   * implementation. *)
  let strings = [""; "hello"; "hello_goodbye"; "<_>"; "«»"; "‡"; "𐆗"] in
  test strings;
  printf "@]";

  [%expect{|
    hash_fold "" -> 0xf255_7dfc_c4e8_fe52_28df_63b7_cc57_c3cbu128
    hash_fold "hello" -> 0x3128_f4fd_508b_45ed_9cac_ecd5_d1b9_3d0eu128
    hash_fold "hello_goodbye" -> 0xd6d0_eb4d_f49b_3d1d_2f69_bc41_817e_6fe0u128
    hash_fold "<_>" -> 0xde67_b895_c176_2ed2_1ac3_366f_7368_8471u128
    hash_fold "«»" -> 0x5caf_027a_5927_7a86_2c22_2383_e256_bcd7u128
    hash_fold "‡" -> 0xfc24_ad42_f613_537a_e903_d82f_077b_81b7u128
    hash_fold "𐆗" -> 0x9fbc_28bd_d639_1c44_1305_4901_c3c5_ee46u128
    |}]

let%expect_test "hash_fold empty" =
  let hash_empty state = begin
    state
    |> hash_fold ""
  end in
  let e1 =
    Hash.State.empty
    |> hash_empty
  in
  let e2 =
    Hash.State.empty
    |> hash_empty
    |> hash_empty
  in
  assert U128.((Hash.t_of_state e1) <> (Hash.t_of_state e2));

  [%expect{|
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
          printf "cmp %a %a -> %a\n" pp s pp s2 Cmp.pp (cmp s s2)
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

let%expect_test "pp" =
  let open Format in
  let s = "hello" in

  printf "s=\"%s\", pp=%a\n" s pp s;

  [%expect{|
    s="hello", pp="hello"
    |}]

let%expect_test "string" =
  let open Format in

  let s = "hello" in
  let s2 = of_string s in
  let s3 = to_string s2 in

  printf "s=%a, s2=%a, s3=%a\n" pp s pp s2 pp s3;

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
    printf "s=%a, blength=%a, clength=%a, is_empty=%B\n"
      pp s
      Uns.pp (blength s)
      Uns.pp (clength s)
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
      match Uns.(i = (blength s)) with
      | true -> ()
      | false -> begin
          printf " %a" Byte.pp_x (get i s);
          fn (Uns.succ i)
        end
    end in
    printf "s=%a:" pp s;
    fn 0;
    printf "\n";
  );

  [%expect{|
    s="":
    s="<_>": 0x3cu8 0x5fu8 0x3eu8
    s="«»": 0xc2u8 0xabu8 0xc2u8 0xbbu8
    s="‡": 0xe2u8 0x80u8 0xa1u8
    s="𐆗": 0xf0u8 0x90u8 0x86u8 0x97u8
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
          let () = if Uns.(i_prev <> max_value) then
              for j = 0 to i - i_prev - 1 do
                assert Cursor.((near s ~bindex:(i_prev + j))
                  = (at s ~bindex:i_prev));
              done
          in
          printf "            %a=%s\n"
            Cursor.pp cursor (of_codepoint (Cursor.rget cursor));
          let cp, cursor' = Cursor.next cursor in
          assert Codepoint.(cp = Cursor.rget cursor);
          assert Cursor.(pred cursor' = cursor);
          fn (Cursor.succ cursor) i
        end
    end in
    printf "cursor fwd:\n";
    fn (Cursor.hd s);
  end in
  let test_rev s = begin
    let rec fn cursor i_prev = begin
      match Cursor.(cursor = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursor.bindex cursor in
          assert Cursor.((at s ~bindex:i) = cursor);
          let () = if Uns.(i_prev <> max_value) then
              for j = 0 to i_prev - i - 1 do
                assert Cursor.((near s ~bindex:(i + j)) = (at s ~bindex:i));
              done
          in
          printf "            %a=%s\n"
            Cursor.pp cursor (of_codepoint (Cursor.lget cursor));
          let cp, cursor' = Cursor.prev cursor in
          assert Codepoint.(cp = Cursor.lget cursor);
          assert Cursor.(succ cursor' = cursor);
          fn (Cursor.pred cursor) i
        end
    end in
    printf "cursor rev:\n";
    fn (Cursor.tl s);
  end in
  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    test_fwd s Uns.max_value;
    test_rev s Uns.max_value;
  );
  printf "@]";

  [%expect{|
    cursor fwd:

    cursor rev:

    cursor fwd:
                {string="<_>«‡𐆗»[_]", bindex=0}=<
                {string="<_>«‡𐆗»[_]", bindex=1}=_
                {string="<_>«‡𐆗»[_]", bindex=2}=>
                {string="<_>«‡𐆗»[_]", bindex=3}=«
                {string="<_>«‡𐆗»[_]", bindex=5}=‡
                {string="<_>«‡𐆗»[_]", bindex=8}=𐆗
                {string="<_>«‡𐆗»[_]", bindex=12}=»
                {string="<_>«‡𐆗»[_]", bindex=14}=[
                {string="<_>«‡𐆗»[_]", bindex=15}=_
                {string="<_>«‡𐆗»[_]", bindex=16}=]

    cursor rev:
                {string="<_>«‡𐆗»[_]", bindex=17}=]
                {string="<_>«‡𐆗»[_]", bindex=16}=_
                {string="<_>«‡𐆗»[_]", bindex=15}=[
                {string="<_>«‡𐆗»[_]", bindex=14}=»
                {string="<_>«‡𐆗»[_]", bindex=12}=𐆗
                {string="<_>«‡𐆗»[_]", bindex=8}=‡
                {string="<_>«‡𐆗»[_]", bindex=5}=«
                {string="<_>«‡𐆗»[_]", bindex=3}=>
                {string="<_>«‡𐆗»[_]", bindex=2}=_
                {string="<_>«‡𐆗»[_]", bindex=1}=<
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
          printf "             %a=%s\n"
            Cursori.pp cursori (of_codepoint (Cursori.rget cursori));
          fn (Cursori.succ cursori)
        end
    end in
    printf "cursori fwd:\n";
    fn (Cursori.hd s);
  end in

  let test_rev s = begin
    let rec fn cursori = begin
      match Cursori.(cursori = (hd s)) with
      | true -> printf "\n"
      | false -> begin
          let i = Cursori.cindex cursori in
          assert Cursori.((at s ~cindex:i) = cursori);
          printf "             %a=%s\n"
            Cursori.pp cursori (of_codepoint (Cursori.lget cursori));
          fn (Cursori.pred cursori)
        end
    end in
    printf "cursori rev:\n";
    fn (Cursori.tl s);
  end in

  let strs = [
    "";
    "<_>«‡𐆗»[_]";
  ] in
  printf "@[<h>";
  List.iter strs ~f:(fun s ->
    test_fwd s;
    test_rev s;
  );
  printf "@]";

  [%expect{|
    cursori fwd:

    cursori rev:

    cursori fwd:
                 {cursor={string="<_>«‡𐆗»[_]", bindex=0}, cindex=0}=<
                 {cursor={string="<_>«‡𐆗»[_]", bindex=1}, cindex=1}=_
                 {cursor={string="<_>«‡𐆗»[_]", bindex=2}, cindex=2}=>
                 {cursor={string="<_>«‡𐆗»[_]", bindex=3}, cindex=3}=«
                 {cursor={string="<_>«‡𐆗»[_]", bindex=5}, cindex=4}=‡
                 {cursor={string="<_>«‡𐆗»[_]", bindex=8}, cindex=5}=𐆗
                 {cursor={string="<_>«‡𐆗»[_]", bindex=12}, cindex=6}=»
                 {cursor={string="<_>«‡𐆗»[_]", bindex=14}, cindex=7}=[
                 {cursor={string="<_>«‡𐆗»[_]", bindex=15}, cindex=8}=_
                 {cursor={string="<_>«‡𐆗»[_]", bindex=16}, cindex=9}=]

    cursori rev:
                 {cursor={string="<_>«‡𐆗»[_]", bindex=17}, cindex=10}=]
                 {cursor={string="<_>«‡𐆗»[_]", bindex=16}, cindex=9}=_
                 {cursor={string="<_>«‡𐆗»[_]", bindex=15}, cindex=8}=[
                 {cursor={string="<_>«‡𐆗»[_]", bindex=14}, cindex=7}=»
                 {cursor={string="<_>«‡𐆗»[_]", bindex=12}, cindex=6}=𐆗
                 {cursor={string="<_>«‡𐆗»[_]", bindex=8}, cindex=5}=‡
                 {cursor={string="<_>«‡𐆗»[_]", bindex=5}, cindex=4}=«
                 {cursor={string="<_>«‡𐆗»[_]", bindex=3}, cindex=3}=>
                 {cursor={string="<_>«‡𐆗»[_]", bindex=2}, cindex=2}=_
                 {cursor={string="<_>«‡𐆗»[_]", bindex=1}, cindex=1}=<
    |}]

let%expect_test "fold_until" =
  let open Format in
  let test_fold_until s = begin
    printf "fold_until %a ->" pp s;
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
    printf "fold_right_until %a ->" pp s;
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
    printf "foldi_until %a ->" pp s;
    let () = foldi_until s ~init:() ~f:(fun i _ cp ->
      let until = Codepoint.(cp = (of_char 'c')) in
      printf " %a:%s" Uns.pp i (of_codepoint cp);
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
    printf "fold %a ->" pp s;
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
    printf "fold_right %a ->" pp s;
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
    printf "foldi %a ->" pp s;
    let () = foldi s ~init:() ~f:(fun i _ cp ->
      printf " %a:%s" Uns.pp i (of_codepoint cp)) in
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
    printf "iter %a ->" pp s;
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
    printf "iteri %a ->" pp s;
    let () = iteri s ~f:(fun i cp ->
      printf " %a:%s" Uns.pp i (of_codepoint cp)
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
    printf "for_any %a '%s' -> %B\n" pp s (of_codepoint cp) (for_any s ~f);
    printf "for_all %a '%s' -> %B\n" pp s (of_codepoint cp) (for_all s ~f);
    printf "mem %a '%s' -> %B\n" pp s (of_codepoint cp) (mem cp s);
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
    printf "find %a -> %s\n" pp s (match find s ~f with
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
    printf "find_map %a -> %s\n" pp s (match find_map s ~f with
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
    printf "min_elm %a -> %s\n" pp s (match min_elm s ~cmp with
      | None -> "None"
      | Some cp -> "'" ^ (of_codepoint cp) ^ "'"
    );
    printf "max_elm %a -> %s\n" pp s (match max_elm s ~cmp with
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
  printf "init -> %a\n" pp (init (Array.length codepoints) ~f:(fun i ->
    Array.get i codepoints
  ));
  printf "of_codepoint -> %a\n" pp (of_codepoint (Codepoint.of_char 'a'));

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
    printf "list: %a -> ... -> %a\n" pp s pp s';

    let s = of_list_rev l in
    let l' = to_list_rev s in
    let s' = of_list_rev l' in
    printf "list_rev: %a -> ... -> %a\n" pp s pp s';
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
    printf "array: %a -> ... -> %a\n" pp s pp s';
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
  printf "map: %a -> %a\n" pp s pp (map s ~f:(fun cp ->
    Codepoint.of_uns ((Codepoint.to_uns cp) - 32)));
  printf "mapi: %a -> %a\n" pp s pp (mapi s ~f:(fun i cp ->
    match (bit_and i 0x1) with
    | 0 -> cp
    | 1 -> Codepoint.of_uns ((Codepoint.to_uns cp) - 32)
    | _ -> not_reached ()
  ));
  let s = "a:b:cd:e" in
  printf "tr: %a -> %a\n" pp s pp (tr s ~target:Codepoint.(of_char ':')
    ~replacement:Codepoint.(of_char ' '));
  printf "filter: %a -> %a\n" pp s pp (filter s ~f:(fun codepoint ->
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
    printf "rev %a -> %a\n" pp s pp (rev s);
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
    | 0x80 -> ()
    | _ -> begin
        printf "%a -> \"%s\"\n"
          Uns.pp_x i (escaped (of_codepoint Codepoint.(of_uns i)));
        fn (Uns.succ i)
      end
  end in
  fn 0;

  [%expect{|
    0x0000000000000000 -> "\u{0}"
    0x0000000000000001 -> "\u{1}"
    0x0000000000000002 -> "\u{2}"
    0x0000000000000003 -> "\u{3}"
    0x0000000000000004 -> "\u{4}"
    0x0000000000000005 -> "\u{5}"
    0x0000000000000006 -> "\u{6}"
    0x0000000000000007 -> "\u{7}"
    0x0000000000000008 -> "\u{8}"
    0x0000000000000009 -> "\t"
    0x000000000000000a -> "\n"
    0x000000000000000b -> "\u{b}"
    0x000000000000000c -> "\u{c}"
    0x000000000000000d -> "\r"
    0x000000000000000e -> "\u{e}"
    0x000000000000000f -> "\u{f}"
    0x0000000000000010 -> "\u{10}"
    0x0000000000000011 -> "\u{11}"
    0x0000000000000012 -> "\u{12}"
    0x0000000000000013 -> "\u{13}"
    0x0000000000000014 -> "\u{14}"
    0x0000000000000015 -> "\u{15}"
    0x0000000000000016 -> "\u{16}"
    0x0000000000000017 -> "\u{17}"
    0x0000000000000018 -> "\u{18}"
    0x0000000000000019 -> "\u{19}"
    0x000000000000001a -> "\u{1a}"
    0x000000000000001b -> "\u{1b}"
    0x000000000000001c -> "\u{1c}"
    0x000000000000001d -> "\u{1d}"
    0x000000000000001e -> "\u{1e}"
    0x000000000000001f -> "\u{1f}"
    0x0000000000000020 -> " "
    0x0000000000000021 -> "!"
    0x0000000000000022 -> "\""
    0x0000000000000023 -> "#"
    0x0000000000000024 -> "$"
    0x0000000000000025 -> "%"
    0x0000000000000026 -> "&"
    0x0000000000000027 -> "'"
    0x0000000000000028 -> "("
    0x0000000000000029 -> ")"
    0x000000000000002a -> "*"
    0x000000000000002b -> "+"
    0x000000000000002c -> ","
    0x000000000000002d -> "-"
    0x000000000000002e -> "."
    0x000000000000002f -> "/"
    0x0000000000000030 -> "0"
    0x0000000000000031 -> "1"
    0x0000000000000032 -> "2"
    0x0000000000000033 -> "3"
    0x0000000000000034 -> "4"
    0x0000000000000035 -> "5"
    0x0000000000000036 -> "6"
    0x0000000000000037 -> "7"
    0x0000000000000038 -> "8"
    0x0000000000000039 -> "9"
    0x000000000000003a -> ":"
    0x000000000000003b -> ";"
    0x000000000000003c -> "<"
    0x000000000000003d -> "="
    0x000000000000003e -> ">"
    0x000000000000003f -> "?"
    0x0000000000000040 -> "@"
    0x0000000000000041 -> "A"
    0x0000000000000042 -> "B"
    0x0000000000000043 -> "C"
    0x0000000000000044 -> "D"
    0x0000000000000045 -> "E"
    0x0000000000000046 -> "F"
    0x0000000000000047 -> "G"
    0x0000000000000048 -> "H"
    0x0000000000000049 -> "I"
    0x000000000000004a -> "J"
    0x000000000000004b -> "K"
    0x000000000000004c -> "L"
    0x000000000000004d -> "M"
    0x000000000000004e -> "N"
    0x000000000000004f -> "O"
    0x0000000000000050 -> "P"
    0x0000000000000051 -> "Q"
    0x0000000000000052 -> "R"
    0x0000000000000053 -> "S"
    0x0000000000000054 -> "T"
    0x0000000000000055 -> "U"
    0x0000000000000056 -> "V"
    0x0000000000000057 -> "W"
    0x0000000000000058 -> "X"
    0x0000000000000059 -> "Y"
    0x000000000000005a -> "Z"
    0x000000000000005b -> "["
    0x000000000000005c -> "\\"
    0x000000000000005d -> "]"
    0x000000000000005e -> "^"
    0x000000000000005f -> "_"
    0x0000000000000060 -> "`"
    0x0000000000000061 -> "a"
    0x0000000000000062 -> "b"
    0x0000000000000063 -> "c"
    0x0000000000000064 -> "d"
    0x0000000000000065 -> "e"
    0x0000000000000066 -> "f"
    0x0000000000000067 -> "g"
    0x0000000000000068 -> "h"
    0x0000000000000069 -> "i"
    0x000000000000006a -> "j"
    0x000000000000006b -> "k"
    0x000000000000006c -> "l"
    0x000000000000006d -> "m"
    0x000000000000006e -> "n"
    0x000000000000006f -> "o"
    0x0000000000000070 -> "p"
    0x0000000000000071 -> "q"
    0x0000000000000072 -> "r"
    0x0000000000000073 -> "s"
    0x0000000000000074 -> "t"
    0x0000000000000075 -> "u"
    0x0000000000000076 -> "v"
    0x0000000000000077 -> "w"
    0x0000000000000078 -> "x"
    0x0000000000000079 -> "y"
    0x000000000000007a -> "z"
    0x000000000000007b -> "{"
    0x000000000000007c -> "|"
    0x000000000000007d -> "}"
    0x000000000000007e -> "~"
    0x000000000000007f -> "\u{7f}"
    |}]

let%expect_test "find" =
  let open Format in
  let test_find s cp = begin
    printf "lfind %a '%s' -> %s\n" pp s (of_codepoint cp)
      (match lfind cp s with
        | None -> "<not found>"
        | Some cursor -> asprintf "%a" Uns.pp (Cursor.bindex cursor)
      );
    printf "contains %a '%s' -> %B\n" pp s (of_codepoint cp) (contains cp s);
    printf "rfind %a '%s' -> %s\n" pp s (of_codepoint cp)
      (match rfind cp s with
        | None -> "<not found>"
        | Some cursor -> asprintf "%a" Uns.pp (Cursor.bindex cursor)
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
  let patterns = [
    "";
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
  printf "@[";
  List.iter patterns ~f:(fun pattern ->
    let p = Slice.Pattern.create (Slice.of_string pattern) in
    printf "%a@\n" Slice.Pattern.pp p;
    printf "     in_:%a@\n" pp s;

    let print_matches matches = begin
      match matches with
      | [] -> ()
      | matches -> begin
          let _ = List.fold matches ~init:None ~f:(fun prev cursor ->
            assert (match prev with
              | None -> true
              | Some c -> Uns.((Cursor.bindex c) < (Cursor.bindex cursor))
            );
            let offset = match prev with
              | None -> (Cursor.bindex cursor) + 2
              | Some c -> (Cursor.bindex cursor) - (Cursor.bindex c)
            in
            printf "%*s" offset "|";
            Some cursor
          ) in
          ()
        end
    end in

    printf "     all:";
    print_matches (substr_find_all s ~may_overlap:true ~pattern);
    printf "@\n";

    printf "disjoint:";
    print_matches (substr_find_all s ~may_overlap:false ~pattern);
    printf "@\n";

    printf "   first:";
    let () = match substr_find s ~pattern with
      | None -> ()
      | Some cursor ->
        printf " %*s" (succ (Cursor.bindex cursor)) "|";
    in
    printf "@\n";
    printf "@\n";
    ()
  );
  printf "@]";

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
    printf "s/%s/%s/ %a -> %a\n"
      pattern with_ pp in_ pp (substr_replace_first in_ ~pattern ~with_);
    printf "s/%s/%s/g %a -> %a\n"
      pattern with_ pp in_ pp (substr_replace_all in_ ~pattern ~with_);
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
    printf "%a |slice| -> %a\n"
      pp s pp (pare ~base:(Cursor.hd s) ~past:(Cursor.tl s));
    let () = match clength s with
      | 0 -> ()
      | _ -> begin
          printf "%a .|slice| -> %a\n"
            pp s pp (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(tl s));
          printf "%a |slice|. -> %a\n"
            pp s pp (pare ~base:Cursor.(hd s) ~past:Cursor.(pred (tl s)))
        end
    in
    let () = match clength s with
      | 0 -> ()
      | 1 -> ()
      | _ ->
        printf "%a .|slice|. -> %a\n"
          pp s pp (pare ~base:Cursor.(succ (hd s)) ~past:Cursor.(pred (tl s)))
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
    for i = 0 to (clength s) + 1 do
      printf "prefix %a %a -> %a\n" pp s Uns.pp i pp (prefix i s);
      printf "suffix %a %a -> %a\n" pp s Uns.pp i pp (suffix i s);
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
    printf "is_prefix %a ~prefix:%a -> %B\n" pp s pp prefix
      (is_prefix s ~prefix);
    printf "chop_prefix %a ~prefix:%a -> %s\n" pp s pp prefix
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
    printf "is_suffix %a ~suffix:%a -> %B\n" pp s pp suffix
      (is_suffix s ~suffix);
    printf "chop_suffix %a ~suffix:%a -> %s\n" pp s pp suffix
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
    printf "lstrip %a -> %a\n" pp s pp (lstrip ?drop s);
    printf "rstrip %a -> %a\n" pp s pp (rstrip ?drop s);
    printf "strip %a -> %a\n" pp s pp (strip ?drop s);
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
    printf "split %a -> [" pp s;
    List.iteri (split s ~f) ~f:(fun i substr ->
      if Uns.(i > 0) then printf "; ";
      printf "%a" pp substr
    );
    printf "]\n";

    printf "split_rev %a -> [" pp s;
    List.iteri (split_rev s ~f)~f:(fun i substr ->
      if Uns.(i > 0) then printf "; ";
      printf "%a" pp substr
    );
    printf "]\n";

    let s1, s2 = lsplit2_hlt s ~on:cp in
    printf "lsplit2_hlt %a -> (%a, %a)\n" pp s pp s1 pp s2;

    let s1, s2 = rsplit2_hlt s ~on:cp in
    printf "rsplit2_hlt %a -> (%a, %a)\n" pp s pp s1 pp s2;
  end in
  test_split ";a::bc;de;" (fun cp -> Codepoint.(cp = (kv (Char.code ':'))))
    (Codepoint.kv 0x3a);
  test_split ":a::bc;de:" (fun cp -> Codepoint.(cp = (kv (Char.code ':'))))
    (Codepoint.kv 0x3b);
  test_split ":a::bc;de;" (fun cp ->
    match Codepoint.to_uns cp with
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
      if Uns.(i > 0) then printf "; ";
      printf "%S" substr
    );
    printf "]\n";

    printf "split_lines_rev %S -> [" s;
    List.iteri (split_lines_rev s)~f:(fun i substr ->
      if Uns.(i > 0) then printf "; ";
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
