open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str "_";
  scan_str "A B C D E F G H I J K L M N O P Q R S T U V W X Y Z";
  scan_str "a b c d e f g h i j k l m n o p q r s t u v w x y z";
  scan_str "_ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_'";
  scan_str "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_'";
  scan_str "A _A __A ___A";
  scan_str "a _a __a ___a";
  scan_str "__ _0 _' __0 __'";

  scan_str "e ef eff effe effec effect effects";

  scan_str "and";
  scan_str "also";
  scan_str "as";
  scan_str "conceal";
  scan_str "effect";
  scan_str "else";
  scan_str "expose";
  scan_str "external";
  scan_str "false";
  scan_str "fn";
  scan_str "function";
  scan_str "if";
  scan_str "import";
  scan_str "include";
  scan_str "lazy";
  scan_str "let";
  scan_str "match";
  scan_str "mutability";
  scan_str "of";
  scan_str "open";
  scan_str "or";
  scan_str "rec";
  scan_str "then";
  scan_str "true";
  scan_str "type";
  scan_str "val";
  scan_str "when";
  scan_str "with"

let _ = test ()
