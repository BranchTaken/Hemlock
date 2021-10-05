open Rudiments

module Error = struct
  type t = uns

  (* external to_string_get_length: t -> uns *)
  external to_string_get_length: t -> uns = "hemlock_file_error_to_string_get_length"

  (* external to_string_inner: uns -> Bytes.t -> t $-> unit *)
  external to_string_inner: uns -> Bytes.t -> t -> unit = "hemlock_file_error_to_string_inner"

  let of_value value =
    Uns.bits_of_sint Sint.(neg value)

  let to_string t =
    let n = to_string_get_length t in
    let bytes = Array.init n ~f:(fun _ -> Byte.kv 0L) in
    let () = to_string_inner n bytes t in
    Bytes.to_string_hlt bytes
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

(* external of_path_inner: Flag.t -> uns -> uns -> uns -> byte /_ array $-> sint *)
external of_path_inner: Flag.t -> uns -> uns -> uns -> Bytes.t -> sint =
  "hemlock_file_of_path_inner"

let of_path ?(flag=Flag.R_O) ?(mode=0o660L) path =
  let value = of_path_inner flag mode (Bytes.Cursor.index (Bytes.Slice.base path))
    (Bytes.Cursor.index (Bytes.Slice.past path)) (Bytes.Slice.container path) in
  match Sint.(value < kv 0L) with
  | false -> Ok (Uns.bits_of_sint value)
  | true -> Error (Uns.bits_of_sint Sint.(neg value))

let of_path_hlt ?flag ?mode path =
  match of_path ?flag ?mode path with
  | Ok t -> t
  | Error error -> halt (Error.to_string error)

(* external stdin_inner: unit $-> uns *)
external stdin_inner: unit -> t = "hemlock_file_stdin_inner"

let stdin =
  stdin_inner ()

(* external stdout_inner: unit $-> uns *)
external stdout_inner: unit -> t = "hemlock_file_stdout_inner"

let stdout =
  stdout_inner ()

(* external stderr_inner: unit $-> uns *)
external stderr_inner: unit -> t = "hemlock_file_stderr_inner"

let stderr =
  stderr_inner ()

(* external close_inner: t $-> sint *)
external close_inner: t -> sint = "hemlock_file_close_inner"

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

let read_base inner buffer t =
  let value = inner (Bytes.Cursor.index (Bytes.Slice.base buffer)) (Bytes.Cursor.index
      (Bytes.Slice.past buffer)) (Bytes.Slice.container buffer) t in
  match Sint.(value < kv 0L) with
  | true -> Error (Uns.bits_of_sint Sint.(neg value))
  | false ->
    Ok (Bytes.Slice.of_cursors ~base:(Bytes.Slice.base buffer) ~past:(Bytes.Cursor.seek value
        (Bytes.Slice.base buffer))
    )

(* external read_inner: uns -> uns -> byte array -> t $-> sint *)
external read_inner: uns -> uns -> Bytes.t -> t -> sint = "hemlock_file_read_inner"

let read ?(n=read_n) t =
  let bytes = Array.init n ~f:(fun _ -> Byte.kv 0L) in
  let buffer = Bytes.Slice.of_container bytes in
  read_base read_inner buffer t

let read_hlt ?n t =
  match read ?n t with
  | Ok buffer -> buffer
  | Error error -> let _ = close_hlt t in halt (Error.to_string error)

(* external read_into_inner: uns -> uns -> byte !&array -> t $-> sint *)
external read_into_inner: uns -> uns -> Bytes.t -> t -> sint = "hemlock_file_read_into_inner"

let read_into buffer t =
  match read_base read_into_inner buffer t with
  | Ok _ -> None
  | Error error -> Some error

let read_into_hlt buffer t =
  match read_into buffer t with
  | None -> ()
  | Some error -> begin
      let _ = close_hlt t in
      halt (Error.to_string error)
    end

(* external write_inner: uns -> uns -> bytes -> t $-> sint *)
external write_inner: uns -> uns -> Bytes.t -> t -> sint = "hemlock_file_write_inner"

let rec write buffer t =
  match Bytes.Cursor.index (Bytes.Slice.base buffer) < Bytes.Cursor.index (Bytes.Slice.past buffer)
  with
  | false -> None
  | true -> begin
      let value = write_inner (Bytes.Cursor.index (Bytes.Slice.base buffer)) (Bytes.Cursor.index
          (Bytes.Slice.past buffer)) (Bytes.Slice.container buffer) t in
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

(* external seek_inner: sint -> t $-> sint *)
external seek_inner: sint -> t -> sint = "hemlock_file_seek_inner"

let seek = seek_base seek_inner

let seek_hlt = seek_hlt_base seek_inner

(* external seek_hd_inner: sint -> t $-> sint *)
external seek_hd_inner: sint -> t -> sint = "hemlock_file_seek_hd_inner"

let seek_hd = seek_base seek_hd_inner

let seek_hd_hlt = seek_hlt_base seek_hd_inner

(* external seek_tl_inner: sint -> t $-> sint *)
external seek_tl_inner: sint -> t -> sint = "hemlock_file_seek_tl_inner"

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
          match Bytes.Cursor.index (Bytes.Slice.base buffer) < Bytes.Cursor.index (Bytes.Slice.past
              buffer) with
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
