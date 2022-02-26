open Rudiments

module Error = struct
  type t = uns

  external to_string_get_length: t -> uns = "hm_basis_file_error_to_string_get_length"

  external to_string_inner: uns -> Stdlib.Bytes.t -> t -> unit =
    "hm_basis_file_error_to_string_inner"

  let of_neg_errno neg_errno =
    Uns.bits_of_sint Sint.(neg neg_errno)

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

external stdin_inner: unit -> t = "hm_basis_file_stdin_inner"

let stdin =
  stdin_inner ()

external stdout_inner: unit -> t = "hm_basis_file_stdout_inner"

let stdout =
  stdout_inner ()

external stderr_inner: unit -> t = "hm_basis_file_stderr_inner"

let stderr =
  stderr_inner ()

external user_data_decref: uns -> unit = "hm_basis_file_user_data_decref"
external complete_inner: uns -> sint = "hm_basis_file_complete_inner"

let register_user_data_finalizer user_data =
  match user_data = 0L with
  | true -> user_data
  | false -> begin
      let () = Stdlib.Gc.finalise user_data_decref user_data in
      user_data
    end

module Open = struct
  type file = t
  type t = uns

  external submit_inner: Flag.t -> uns -> Stdlib.Bytes.t -> (sint * t) =
    "hm_basis_file_open_submit_inner"

  let submit ?(flag=Flag.R_O) ?(mode=0o660L) path =
    let path_bytes = bytes_of_slice (Path.to_bytes path) in
    let value, t = submit_inner flag mode path_bytes in
    let t = register_user_data_finalizer t in
    match Sint.(value < kv 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> Ok t

  let submit_hlt ?(flag=Flag.R_O) ?(mode=0o660L) path =
    match submit ~flag ~mode path with
    | Error error -> halt (Error.to_string error)
    | Ok t -> t

  let complete t =
    let value = complete_inner t in
    match Sint.(value < 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> Ok (Uns.bits_of_sint value)

  let complete_hlt t =
    match complete t with
    | Ok t -> t
    | Error error -> halt (Error.to_string error)
end

let of_path ?flag ?mode path =
  match Open.submit ?flag ?mode path with
  | Error error -> Error error
  | Ok open' -> Open.complete open'

let of_path_hlt ?flag ?mode path =
  Open.(submit_hlt ?flag ?mode path |> complete_hlt)

module Close = struct
  type file = t
  type t = uns

  external submit_inner: file -> (sint * t) = "hm_basis_file_close_submit_inner"

  let submit file =
    let value, t = submit_inner file in
    let t = register_user_data_finalizer t in
    match Sint.(value < kv 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> Ok t

  let submit_hlt file =
    match submit file with
    | Error error -> halt (Error.to_string error)
    | Ok t -> t

  let complete t =
    let value = complete_inner t in
    match Sint.(value < kv 0L) with
    | true -> Some (Error.of_neg_errno value)
    | false -> None

  let complete_hlt t =
    match complete t with
    | None -> ()
    | Some error -> halt (Error.to_string error)
end

let close t =
  match Close.submit t with
  | Error error -> Some error
  | Ok close -> Close.complete close

let close_hlt t =
  Close.(submit_hlt t |> complete_hlt)

module Read = struct
  type file = t
  type inner = uns
  type t = {
    inner: inner;
    buffer: Bytes.Slice.t;
  }

  let default_n = 1024L

  external submit_inner: uns -> file -> (sint * inner) = "hm_basis_file_read_submit_inner"

  let submit ?n ?buffer file =
    let n, buffer = begin
      match n with
      | None -> begin
          match buffer with
          | None -> default_n, Bytes.Slice.init (Array.init (0L =:< default_n)
            ~f:(fun _ -> Byte.kv 0L))
          | Some buffer -> Bytes.Slice.length buffer, buffer
        end
      | Some n -> begin
          match buffer with
          | None -> n, Bytes.Slice.init (Array.init (0L =:< n) ~f:(fun _ -> Byte.kv 0L))
          | Some buffer -> (Uns.min n (Bytes.Slice.length buffer)), buffer
        end
    end in
    let value, inner = submit_inner n file in
    let inner = register_user_data_finalizer inner in
    match Sint.(value < kv 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> Ok {inner; buffer}

  let submit_hlt ?n ?buffer file =
    match submit ?n ?buffer file with
    | Error error -> halt (Error.to_string error)
    | Ok t -> t

  external complete_inner: Stdlib.Bytes.t -> inner -> sint =
    "hm_basis_file_read_complete_inner"

  let complete t =
    let base = Bytes.(Cursor.index (Slice.base t.buffer)) in
    let bytes = Stdlib.Bytes.create (Int64.to_int (Bytes.Slice.length t.buffer)) in
    let value = complete_inner bytes t.inner in
    match Sint.(value < kv 0L) with
    | true -> Error (Uns.bits_of_sint Sint.(neg value))
    | false -> begin
        let range = (base =:< (base + (Uns.bits_of_sint value))) in
        let container = Bytes.Slice.container t.buffer in
        Range.Uns.iter range ~f:(fun i ->
          Array.set_inplace i (U8.of_char (Stdlib.Bytes.get bytes (Int64.to_int (i - base))))
            container
        );
        Ok (Bytes.Slice.init ~range (Bytes.Slice.container t.buffer))
      end

  let complete_hlt t =
    match complete t with
    | Ok buffer -> buffer
    | Error error -> halt (Error.to_string error)

end

let read ?n ?buffer t =
  match Read.submit ?n ?buffer t with
  | Error error -> Error error
  | Ok read -> Read.complete read

let read_hlt ?n ?buffer t =
  Read.(submit_hlt ?n ?buffer t |> complete_hlt)

module Write = struct
  type file = t
  type inner = uns
  type t = {
    inner: inner;
    buffer: Bytes.Slice.t;
  }

  external submit_inner: Stdlib.Bytes.t -> file -> (sint * inner) =
    "hm_basis_file_write_submit_inner"

  let submit buffer file =
    let bytes = bytes_of_slice buffer in
    let value, inner = submit_inner bytes file in
    let inner = register_user_data_finalizer inner in
    match Sint.(value < kv 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> Ok {inner; buffer}

  let submit_hlt buffer file =
    match submit buffer file with
    | Error error -> halt (Error.to_string error)
    | Ok t -> t

  let complete t =
    let value = complete_inner t.inner in
    match Sint.(value < kv 0L) with
    | true -> Error (Error.of_neg_errno value)
    | false -> begin
        let base = Bytes.(Slice.base t.buffer |> Cursor.seek (Uns.bits_of_sint value)) in
        let past = Bytes.Slice.past t.buffer in
        let buffer = Bytes.Slice.of_cursors ~base ~past in
        Ok buffer
      end

  let complete_hlt t =
    match complete t with
    | Ok buffer -> buffer
    | Error error -> halt (Error.to_string error)
end

let write buffer t =
  let rec f buffer t = begin
    match Bytes.Slice.length buffer = 0L with
    | true -> None
    | false -> begin
        match Write.submit buffer t with
        | Error error -> Some error
        | Ok write -> begin
            match Write.complete write with
            | Error error -> Some error
            | Ok buffer -> f buffer t
          end
      end
  end in
  f buffer t

let write_hlt buffer t =
  let rec f buffer t = begin
    match Bytes.Slice.length buffer = 0L with
    | true -> ()
    | false -> f Write.(submit_hlt buffer t |> complete_hlt) t
  end in
  f buffer t

let seek_base inner rel_off t =
  let value = inner rel_off t in
  match Sint.(value < kv 0L) with
  | false -> Ok (Uns.bits_of_sint value)
  | true -> Error (Error.of_neg_errno value)

let seek_hlt_base inner rel_off t =
  match seek_base inner rel_off t with
  | Ok base_off -> base_off
  | Error error -> begin
      let _ = close t in
      halt (Error.to_string error)
    end

external seek_inner: sint -> t -> sint = "hm_basis_file_seek_inner"

let seek = seek_base seek_inner

let seek_hlt = seek_hlt_base seek_inner

external seek_hd_inner: sint -> t -> sint = "hm_basis_file_seek_hd_inner"

let seek_hd = seek_base seek_hd_inner

let seek_hd_hlt = seek_hlt_base seek_hd_inner

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
                write_hlt (Bytes.Slice.init ~range:(0L =:< Bytes.Cursor.index pos) buf) t.file;
                t.pos <- Some (Bytes.Cursor.hd buf);
              end;
              ()
            end
          | _ -> ()
        in
        Fmt.Synced t
    end)

  let stdout = of_t stdout

  let stderr = of_t ~bufsize:0L stderr

  let teardown () =
    let _ = Fmt.flush stdout in
    ()
end

external setup_inner: unit -> sint = "hm_basis_file_setup_inner"

let () = begin
  match setup_inner () = 0L with
  | false -> halt "Setup failure"
  | true -> ()
end

external teardown_inner: unit -> unit = "hm_basis_file_teardown_inner"

let () = Stdlib.at_exit (fun () -> let _ = Fmt.teardown () in let _ = teardown_inner () in ())
