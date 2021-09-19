open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str ", ,,";
  scan_str ". ..";
  scan_str "; ;; ;;;";
  scan_str "; : := :: :::"; (* Avoid line directive syntax. *)
  scan_str "]|]|[|[";
  scan_str "}|}|{|{";
  scan_str "{}";
  scan_str {|\^&\&\|};
  scan_str {|!&!!\|};
  scan_str "- -> ->> ->";
  scan_str "~- ~-> ~->> -~->";
  scan_str "> >- >-> >->> >>-> >e-> >{e|mut}->";
  scan_str ">e~-> >{e|mut}~-> ~hlt-> ~{hlt|alloc}-> >e~hlt-> >{e|os}~{hlt|alloc}->";
  scan_str "~f ~- ~-+*/%@$<=>|:.~?";
  scan_str "?x ?? ?-+*/%@$<=>|:.~?";
  printf "@]"

let _ = test ()
