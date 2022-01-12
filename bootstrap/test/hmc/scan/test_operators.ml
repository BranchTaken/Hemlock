open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str "~ ~-+*/%@^$<=>|:.~?";
  scan_str "? ?-+*/%@^$<=>|:.~?";
  scan_str "* *-+*/%@^$<=>|:.~?";
  scan_str "** **-+*/%@^$<=>|:.~?";
  scan_str "% %-+*/%@^$<=>|:.~?";
  scan_str "+ +-+*/%@^$<=>|:.~?";
  scan_str "- --+*/%@^$<=>|:.~?";
  scan_str "@ @-+*/%@^$<=>|:.~?";
  scan_str "^ ^-+*/%@^$<=>|:.~? ^)";
  scan_str "$ $-+*/%@^$<=>|:.~?";
  scan_str "< <-+*/%@^$<=>|:.~?";
  scan_str "= =-+*/%@^$<=>|:.~?";
  scan_str "> >-+*/%@^$<=>|:.~?";
  scan_str "| |-+*/%@^$<=>|:.~?";
  scan_str ": :-+*/%@^$<=>|:.~?";
  scan_str ". .-+*/%@^$<=>|:.~?"

let _ = test ()
