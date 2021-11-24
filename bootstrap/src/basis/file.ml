open Rudiments

module Error = struct
  type t = uns

  (* external to_string_get_length: t -> uns *)
  external to_string_get_length: t -> uns = "hm_basis_file_error_to_string_get_length"

  (* external to_string_inner: uns -> !&bytes -> t >os-> unit *)
  external to_string_inner: uns -> Stdlib.Bytes.t -> t -> unit =
    "hm_basis_file_error_to_string_inner"

  let of_value value =
    Uns.bits_of_sint Sint.(neg value)

  let to_string t =
    let n = to_string_get_length t in
    let bytes = Stdlib.Bytes.create (Int64.to_int n) in
    let () = to_string_inner n bytes t in
    Stdlib.Bytes.to_string bytes
end

module Flag = struct
  (* Modifications to Flag.t must be reflected in file.c. *)
  type t =
    | R_O
    | W
    | W_A
    | W_AO
    | W_C
    | W_O
    | RW
    | RW_A
    | RW_AO
    | RW_C
    | RW_O
end

type t = uns

let bytes_of_slice slice =
  let base = (Bytes.Cursor.index (Bytes.Slice.base slice)) in
  let container = (Bytes.Slice.container slice) in
  Stdlib.Bytes.init (Int64.to_int (Bytes.Slice.length slice)) (fun i ->
    Char.chr (Uns.trunc_to_int (U8.extend_to_uns (Array.get (base + (Int64.of_int i)) container)))
  )

(* external of_path_inner: Flag.t -> uns -> _?bytes >os-> int *)
external of_path_inner: Flag.t -> uns -> Stdlib.Bytes.t -> sint = "hm_basis_file_of_path_inner"

let of_path ?(flag=Flag.R_O) ?(mode=0o660L) path =
  let path_bytes = bytes_of_slice path in
  let value = of_path_inner flag mode path_bytes in
  match Sint.(value < kv 0L) with
  | false -> Ok (Uns.bits_of_sint value)
  | true -> Error (Uns.bits_of_sint Sint.(neg value))

let of_path_hlt ?flag ?mode path =
  match of_path ?flag ?mode path with
  | Ok t -> t
  | Error error -> halt (Error.to_string error)

(* external stdin_inner: uns *)
external stdin_inner: unit -> t = "hm_basis_file_stdin_inner"

let stdin =
  stdin_inner ()

(* external stdout_inner: uns *)
external stdout_inner: unit -> t = "hm_basis_file_stdout_inner"

let stdout =
  stdout_inner ()

(* external stderr_inner: uns *)
external stderr_inner: unit -> t = "hm_basis_file_stderr_inner"

let stderr =
  stderr_inner ()

(* external close_inner: t >os-> int *)
external close_inner: t -> sint = "hm_basis_file_close_inner"

let close t =
  let value = close_inner t in
  match Sint.(value < kv 0L) with
  | false -> None
  | true -> Some (Error.of_value value)

let close_hlt t =
  match close t with
  | None -> ()
  | Some error -> halt (Error.to_string error)

let read_n = 1024L

(* external read_inner: !&bytes -> t >os-> int *)
external read_inner: Stdlib.Bytes.t -> t -> sint = "hm_basis_file_read_inner"

let read_impl buffer t =
  let base = Bytes.(Cursor.index (Slice.base buffer)) in
  let bytes = Stdlib.Bytes.create (Int64.to_int (Bytes.Slice.length buffer)) in
  let value = read_inner bytes t in
  match Sint.(value < kv 0L) with
  | true -> Error (Uns.bits_of_sint Sint.(neg value))
  | false -> begin
      let range = (base =:< (base + (Uns.bits_of_sint value))) in
      let container = Bytes.Slice.container buffer in
      Range.iter range ~f:(fun i ->
        Array.set_inplace i (U8.of_char (Stdlib.Bytes.get bytes (Int64.to_int (i - base))))
          container
      );
      Ok (Bytes.Slice.init ~range (Bytes.Slice.container buffer))
    end

let read ?(n=read_n) t =
  let bytes = Array.init (0L =:< n) ~f:(fun _ -> Byte.kv 0L) in
  let buffer = Bytes.Slice.init bytes in
  read_impl buffer t

let read_hlt ?n t =
  match read ?n t with
  | Ok buffer -> buffer
  | Error error -> let _ = close_hlt t in halt (Error.to_string error)

let read_into buffer t =
  match read_impl buffer t with
  | Ok _ -> None
  | Error error -> Some error

let read_into_hlt buffer t =
  match read_into buffer t with
  | None -> ()
  | Some error -> begin
      let _ = close_hlt t in
      halt (Error.to_string error)
    end

(* external write_inner: _?bytes -> t >os-> sint *)
external write_inner: Stdlib.Bytes.t -> t -> sint = "hm_basis_file_write_inner"

let rec write buffer t =
  match Bytes.Cursor.index (Bytes.Slice.base buffer) < Bytes.Cursor.index (Bytes.Slice.past buffer)
  with
  | false -> None
  | true -> begin
      let bytes = bytes_of_slice buffer in
      let value = write_inner bytes t in
      match Sint.(value < kv 0L) with
      | true -> Some (Error.of_value value)
      | false ->
        write (
          Bytes.Slice.of_cursors ~base:(Bytes.Cursor.seek value (Bytes.Slice.base buffer))
            ~past:(Bytes.Slice.base buffer)
        ) t
    end

let write_hlt buffer t =
  match write buffer t with
  | None -> ()
  | Some error -> begin
      let _ = close t in
      halt (Error.to_string error)
    end

let seek_base inner rel_off t =
  let value = inner rel_off t in
  match Sint.(value < kv 0L) with
  | false -> Ok (Uns.bits_of_sint value)
  | true -> Error (Error.of_value value)

let seek_hlt_base inner rel_off t =
  match seek_base inner rel_off t with
  | Ok base_off -> base_off
  | Error error -> begin
      let _ = close t in
      halt (Error.to_string error)
    end

(* external seek_inner: int -> t >os-> int *)
external seek_inner: sint -> t -> sint = "hm_basis_file_seek_inner"

let seek = seek_base seek_inner

let seek_hlt = seek_hlt_base seek_inner

(* external seek_hd_inner: int -> t >os-> int *)
external seek_hd_inner: sint -> t -> sint = "hm_basis_file_seek_hd_inner"

let seek_hd = seek_base seek_hd_inner

let seek_hd_hlt = seek_hlt_base seek_hd_inner

(* external seek_tl_inner: int -> t >os-> int *)
external seek_tl_inner: sint -> t -> sint = "hm_basis_file_seek_tl_inner"

let seek_tl = seek_base seek_tl_inner

let seek_tl_hlt = seek_hlt_base seek_tl_inner

module Stream = struct
  type file = t
  type t = Bytes.Slice.t Stream.t

  let of_file file =
    let f file = begin
      match read file with
      | Error _ -> None
      | Ok buffer -> begin
          match (Bytes.Slice.length buffer) > 0L with
          | false -> begin
              let _ = close file in
              None
            end
          | true -> Some (buffer, file)
        end
    end in
    Stream.init_indef file ~f

  let write file t =
    let rec fn file t = begin
      match Lazy.force t with
      | Stream.Nil -> None
      | Stream.Cons(buffer, t') -> begin
          match write buffer file with
          | Some error -> Some error
          | None -> fn file t'
        end
    end in
    fn file t

  let write_hlt file t =
    let rec fn file t = begin
      match Lazy.force t with
      | Stream.Nil -> ()
      | Stream.Cons(buffer, t') -> begin
          write_hlt buffer file;
          fn file t'
        end
    end in
    fn file t
end

module Fmt = struct
  let bufsize_default = 4096L

  let of_t ?(bufsize=bufsize_default) t : (module Fmt.Formatter) =
    (module struct
      type nonrec t = {
        file: t;
        bufsize: uns;
        mutable buf: Bytes.t option;
        mutable pos: Bytes.Cursor.t option;
      }

      let state = {
        file=t;
        bufsize;
        buf=None;
        pos=None;
      }

      let fmt s t =
        let slice = Bytes.Slice.of_string_slice (String.C.Slice.of_string s) in
        match Bytes.Slice.length slice, t.bufsize with
        | 0L, _ -> t (* No-op. *)
        | _, 0L -> begin
            (* Unbuffered. *)
            write_hlt slice t.file;
            t
          end
        | _ -> begin
            (* Initialize buf/pos if this is the first write. *)
            let () = match t.buf with
              | None -> begin
                  let buf = Array.init (0L =:< bufsize) ~f:(fun _ -> U8.zero) in
                  t.buf <- Some buf;
                  t.pos <- Some (Bytes.Cursor.hd buf)
                end
              | Some _ -> ()
            in

            (* Fill/flush buf repeatedly until it is not full. *)
            let rec fn t slice = begin
              match t.buf, t.pos with
              | Some buf, Some pos -> begin
                  let pos_index = Bytes.Cursor.index pos in
                  let buf_avail = t.bufsize - pos_index in
                  let slice_length = Bytes.Slice.length slice in
                  match (Bytes.Slice.length slice) < buf_avail with
                  | true -> begin
                      (* Partial fill. *)
                      let buf_range = (pos_index =:< (pos_index + slice_length)) in
                      Array.Slice.blit (Array.Slice.init ~range:(Bytes.Slice.range slice)
                        (Bytes.Slice.container slice)) (Array.Slice.init ~range:buf_range buf);
                      t.pos <- Some (Bytes.Cursor.seek (Uns.bits_to_sint slice_length) pos);
                      t
                    end
                  | false -> begin
                      (* Complete fill. *)
                      let slice_base = Bytes.Slice.base slice in
                      let slice_mid = Bytes.Cursor.seek (Uns.bits_to_sint buf_avail) slice_base in
                      let slice_range =
                        (Bytes.Cursor.index slice_base =:< Bytes.Cursor.index slice_mid) in
                      let buf_range = (pos_index =:< t.bufsize) in
                      Array.Slice.blit
                        (Array.Slice.init ~range:slice_range (Bytes.Slice.container slice))
                        (Array.Slice.init ~range:buf_range buf);
                      (* Flush. *)
                      write_hlt (Bytes.Slice.init ~range:(0L =:< t.bufsize) buf) t.file;

                      let slice' = Bytes.Slice.of_cursors ~base:slice_mid
                          ~past:(Bytes.Slice.past slice) in
                      t.pos <- Some (Bytes.Cursor.hd buf);
                      fn t slice'
                    end
                end
              | _ -> not_reached ()
            end in
            fn t slice
          end

      let sync t =
        let () = match t.buf, t.pos with
          | Some buf, Some pos -> begin
              if (Bytes.Cursor.index pos) > 0L then begin
                write_hlt (Bytes.Slice.init ~range:(0L =:< (Bytes.Cursor.index pos)) buf) t.file;
                t.pos <- Some (Bytes.Cursor.hd buf);
              end;
              ()
            end
          | _ -> ()
        in
        Fmt.Synced t
    end)

  let stdout = of_t stdout

  let () = at_exit (fun () -> let _ = Fmt.flush stdout in ())

  let stderr = of_t ~bufsize:0L stderr
end
