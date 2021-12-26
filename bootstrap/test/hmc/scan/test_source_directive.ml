open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str {|[:"Foo.hm":42:13]|};
  scan_str {|[:"Foo.hm":42]|};
  scan_str {|[:"Foo.hm"]|};
  scan_str {|[:42:13]|};
  scan_str {|[:42]|};
  scan_str {|[:]|};

  scan_str {|[:1:0]|};
  scan_str {|[:10123456789]|};
  scan_str {|[:1] [:2]|};
  scan_str {|[:"Foo.hm"] [:] [:"Bar.hm"]|};
  scan_str {|[:"Foo.hm"] [:"Bar.hm"] [:] [:"Biz.hm"]|};
  scan_str {|[:"Foo.hm":1:42]a
b|};

  scan_str {|[:"A\tB\rC\n\u{44}"]|};
  scan_str {|[:1_0_:0_]|};
  scan_str {|[:_1_0_:_0_]|};
  scan_str {|[:__1__0__:__0__]|};

  (* Errors. *)
  scan_str {|[:"Foo.hm":42:]|};
  scan_str {|[:"Foo.hm"::13]|};
  scan_str {|[::42:13]|};
  scan_str {|[:"Foo.hm"42:13]|};
  scan_str {|[:"Foo.hm:42:13]|};
  scan_str {|[:Foo.hm":42:13]|};
  scan_str {|[:":42:13]|};

  scan_str {|[:"Foo.hm":042:13]|};
  scan_str {|[:"Foo.hm":42:013]|};

  scan_str {|[:0]|};
  scan_str {|[:_0]|};
  scan_str {|[:"Foo.hm":9999999999999999999]|};
  scan_str {|[:"Foo.hm":1:9999999999999999999]|};

  scan_str {|[:1:_00]|};
  scan_str {|[:1:00]|};
  scan_str {|[:1:01]|};

  scan_str {|[:�"Foo.hm":42:13]|};
  scan_str {|[:_"Foo.hm":42:13]|};
  scan_str {|[:"Foo.hm"�:42:13]|};
  scan_str {|[:"Foo.hm"_:42:13]|};
  scan_str {|[:"Foo.hm":�42:13]|};
  scan_str {|[:"Foo.hm":42�:13]|};
  scan_str {|[:"Foo.hm":42:�13]|};
  scan_str {|[:"Foo.hm":42:13�]|};

  scan_str {|[:"Foo.hm":42:13|};
  scan_str {|[:"Foo.hm":42:|};
  scan_str {|[:"Foo.hm":42|};
  scan_str {|[:"Foo.hm":|};
  scan_str {|[:"Foo.hm"|};
  scan_str {|[:"Foo.hm|};
  scan_str {|[:"|};

  scan_str {|[:"\x"]|};
  scan_str {|[:"\u{g}"]|};
  scan_str {|[:"\u{}"]|};
  scan_str {|[:"\u{"]|};
  scan_str {|[:"\u}"]|};
  scan_str {|[:"\ua"]|};
  scan_str {|[:"\u}"]|};
  scan_str {|[:"\u"]|};
  scan_str {|[:"\\]|};
  scan_str {|[:"]|}

let _ = test ()
