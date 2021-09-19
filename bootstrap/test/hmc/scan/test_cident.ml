open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'";
  scan_str "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z";
  printf "@]"

let _ = test ()
