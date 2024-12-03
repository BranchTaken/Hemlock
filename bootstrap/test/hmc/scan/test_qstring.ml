open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str "{||}";
  scan_str "{|
|}";
  scan_str "{|

|}";
  scan_str "{|


|}";
  scan_str "{|a
b|}";
  scan_str "{|a||}";
  scan_str "{||a|}";
  scan_str "{|||}";
  scan_str "{||||}";
  scan_str "{||a}|}";
  scan_str "{|�|}";
  scan_str "{||�|}";

  (* Errors. *)
  scan_str "{|";
  scan_str "{||"

let _ = test ()
