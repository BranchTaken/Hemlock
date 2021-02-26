(** Arbitrary-precision integer (ℤ). *)

open Basis

type t
include IntwIntf.SVI with type t := t

val k_0: t
(** Return constant [0]. *)

val k_1: t
(** Return constant [1]. *)

val k_2: t
(** Return constant [2]. *)

val k_3: t
(** Return constant [3]. *)

val k_4: t
(** Return constant [4]. *)

val k_5: t
(** Return constant [5]. *)

val k_6: t
(** Return constant [6]. *)

val k_7: t
(** Return constant [7]. *)

val k_8: t
(** Return constant [8]. *)

val k_9: t
(** Return constant [9]. *)

val k_a: t
(** Return constant [10] ([0xa]). *)

val k_b: t
(** Return constant [11] ([0xb]). *)

val k_c: t
(** Return constant [12] ([0xc]). *)

val k_d: t
(** Return constant [13] ([0xd]). *)

val k_e: t
(** Return constant [14] ([0xe]). *)

val k_f: t
(** Return constant [15] ([0xf]). *)

val k_g: t
(** Return constant [16] ([0x10]). *)
