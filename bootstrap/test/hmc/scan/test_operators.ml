open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest
open Format

let test () =
  printf "@[<h>";
  scan_str "~ ~-+*/%@$<=>|:.~?";
  scan_str "? ?-+*/%@$<=>|:.~?";
  scan_str "* *-+*/%@$<=>|:.~?";
  scan_str "** **-+*/%@$<=>|:.~?";
  scan_str "% %-+*/%@$<=>|:.~?";
  scan_str "+ +-+*/%@$<=>|:.~?";
  scan_str "- --+*/%@$<=>|:.~?";
  scan_str "@ @-+*/%@$<=>|:.~?";
  scan_str "$ $-+*/%@$<=>|:.~?";
  scan_str "< <-+*/%@$<=>|:.~?";
  scan_str "= =-+*/%@$<=>|:.~?";
  scan_str "> >-+*/%@$<=>|:.~?";
  scan_str "|-+*/%@$<=>|:.~?";
  printf "@]"

let _ = test ()
