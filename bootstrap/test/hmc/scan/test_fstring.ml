open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  (* Minimal. *)
  scan_str {|"%s(^s^)"|};
  scan_str {|"<<<%s(^s^)>>>"|};

  (* Multi-spec. *)
  scan_str {|"%s(^s^)%s(^s^)"|};
  scan_str {|"<<<%s(^s^)^^^%s(^s^)>>>"|};

  (* Recursive. *)
  scan_str {|"%s(^"hi"^)"|};
  scan_str {|"%s(^"%s(^s^)"^)"|};

  (* Singles. *)
  scan_str {|"%'·'s(^s^)"|};
  scan_str {|"%'\''s(^s^)"|};

  scan_str {|"%<s(^s^)"|};
  scan_str {|"%^s(^s^)"|};
  scan_str {|"%>s(^s^)"|};

  scan_str {|"%+i(^42i^)"|};
  scan_str {|"%_i(^42i^)"|};

  scan_str {|"%#u(^42^)"|};

  scan_str {|"%0u(^42^)"|};
  scan_str {|"%012u(^42^)"|};

  scan_str {|"%8u(^42^)"|};
  scan_str {|"%12u(^42^)"|};
  scan_str {|"%*(^8^)u(^42^)"|};

  scan_str {|"%.3r(^42.^)"|};
  scan_str {|"%.=3r(^42.^)"|};
  scan_str {|"%.*(^3^)r(^42.^)"|};

  scan_str {|"%bu(^42^)"|};
  scan_str {|"%ou(^42^)"|};
  scan_str {|"%du(^42^)"|};
  scan_str {|"%xu(^42^)"|};

  scan_str {|"%mr(^42.^)"|};
  scan_str {|"%ar(^42.^)"|};
  scan_str {|"%cr(^42.^)"|};

  scan_str {|"%pu(^42^)"|};

  scan_str {|"%b(^true^)"|};
  scan_str {|"%u(^42^)"|};
  scan_str {|"%u8(^42u8^)"|};
  scan_str {|"%u16(^42u16^)"|};
  scan_str {|"%u32(^42u32^)"|};
  scan_str {|"%u64(^42u64^)"|};
  scan_str {|"%u128(^42u128^)"|};
  scan_str {|"%u256(^42u256^)"|};
  scan_str {|"%u512(^42u512^)"|};
  scan_str {|"%i(^42i^)"|};
  scan_str {|"%i8(^42i8^)"|};
  scan_str {|"%i16(^42i16^)"|};
  scan_str {|"%i32(^42i32^)"|};
  scan_str {|"%i64(^42i64^)"|};
  scan_str {|"%i128(^42i128^)"|};
  scan_str {|"%i256(^42i256^)"|};
  scan_str {|"%i512(^42i512^)"|};
  scan_str {|"%n(^42n^)"|};
  scan_str {|"%z(^42z^)"|};
  scan_str {|"%r(^42r^)"|};
  scan_str {|"%r32(^42r32^)"|};
  scan_str {|"%r64(^42r64^)"|};
  scan_str {|"%c(^'c'^)"|};
  scan_str {|"%s(^"string"^)"|};
  scan_str {|"%f(^Option.pp Uns.pp^)(^Some 42^)"|};

  scan_str {|"%u=(^x^)"|};
  scan_str {|"%u =(^x^)"|};
  scan_str {|"%u = (^x^)"|};
  scan_str {|"%u= (^x^)"|};
  scan_str {|"%u  =  (^x^)"|};
  scan_str {|"%u-+*/%@$<=>|:.~?(^x^)"|};

  (* All. *)
  scan_str {|"%'·'^+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0*(^8^).=*(^3^)xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0*(^8^).=*(^3^)xmpf(^pp^)=(^42.^)"|};

  (* Skips. *)
  scan_str {|"%'·'+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'8.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'.3xmpr=(^42.^)"|};
  scan_str {|"%'·'xmpr=(^42.^)"|};
  scan_str {|"%'·'mpr=(^42.^)"|};
  scan_str {|"%'·'pr=(^42.^)"|};
  scan_str {|"%'·'r=(^42.^)"|};

  scan_str {|"%'·'^#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^8.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^.3xmpr=(^42.^)"|};
  scan_str {|"%'·'^xmpr=(^42.^)"|};
  scan_str {|"%'·'^mpr=(^42.^)"|};
  scan_str {|"%'·'^pr=(^42.^)"|};
  scan_str {|"%'·'^r=(^42.^)"|};

  scan_str {|"%'·'^+08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+8.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+.3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+xmpr=(^42.^)"|};
  scan_str {|"%'·'^+mpr=(^42.^)"|};
  scan_str {|"%'·'^+pr=(^42.^)"|};
  scan_str {|"%'·'^+r=(^42.^)"|};

  scan_str {|"%'·'^+#8.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#.3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#mpr=(^42.^)"|};
  scan_str {|"%'·'^+#pr=(^42.^)"|};
  scan_str {|"%'·'^+#r=(^42.^)"|};

  scan_str {|"%'·'^+#0.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0.3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0mpr=(^42.^)"|};
  scan_str {|"%'·'^+#0pr=(^42.^)"|};
  scan_str {|"%'·'^+#0r=(^42.^)"|};

  scan_str {|"%'·'^+#08.3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08mpr=(^42.^)"|};
  scan_str {|"%'·'^+#08pr=(^42.^)"|};
  scan_str {|"%'·'^+#08r=(^42.^)"|};

  scan_str {|"%'·'^+#08.=3mpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3pr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3r=(^42.^)"|};

  scan_str {|"%'·'^+#08.3mpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.3pr=(^42.^)"|};
  scan_str {|"%'·'^+#08.3r=(^42.^)"|};

  scan_str {|"%'·'^+#08.=3xpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xr=(^42.^)"|};

  scan_str {|"%'·'^+#08.=3xmr=(^42.^)"|};

  (* Unambiguous. *)
  scan_str {|"%b(^true^)"|};
  scan_str {|"%b=(^b^)"|};
  scan_str {|"%bb(^true^)"|}; (* Valid syntax, won't compile (no ~radix for Bool). *)
  scan_str {|"%bu(^42^)"|};

  scan_str {|"%c(^'c'^)"|};
  scan_str {|"%c=(^c^)"|};
  scan_str {|"%cc(^'c'^)"|}; (* Valid syntax, won't compile (no ~notation for Codepoint). *)
  scan_str {|"%cr(^Real.pi^)"|};

  (* Errors. *)
  scan_str {|"%'·''·'^+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^^+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^++#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+##08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#008.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08..=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.==3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3.xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xxmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmppr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmprr=(^42.^)"|};

  scan_str {|"%�'·'^+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'�^+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^�+#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+�#08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#�08.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#0�8.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08�.=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.�=3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=�3xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3�xmpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3x�mpr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xm�pr=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmp�r=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmpr�=(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmpr=�(^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmpr=(�^42.^)"|};
  scan_str {|"%'·'^+#08.=3xmpr=(^42.^�)"|};

  scan_str {|"%'·'^+#08.=3xmpr=(^42.^)|};
  scan_str {|"%'·'^+#08.=3xmpr=(^42.^|};
  scan_str {|"%'·'^+#08.=3xmpr=(^42.|};
  scan_str {|"%'·'^+#08.=3xmpr=(^|};
  scan_str {|"%'·'^+#08.=3xmpr=(|};
  scan_str {|"%'·'^+#08.=3xmpr=|};
  scan_str {|"%'·'^+#08.=3xmpr|};
  scan_str {|"%'·'^+#08.=3xmp|};
  scan_str {|"%'·'^+#08.=3xm|};
  scan_str {|"%'·'^+#08.=3x|};
  scan_str {|"%'·'^+#08.=3|};
  scan_str {|"%'·'^+#08.=|};
  scan_str {|"%'·'^+#08.|};
  scan_str {|"%'·'^+#08|};
  scan_str {|"%'·'^+#0|};
  scan_str {|"%'·'^+#|};
  scan_str {|"%'·'^+|};
  scan_str {|"%'·'^|};
  scan_str {|"%'·'|};
  scan_str {|"%'·|};
  scan_str {|"%'|};
  scan_str {|"%|};
  scan_str {|"%'·'^+#08.=*|};
  scan_str {|"%'·'^+#0*|};

  scan_str {|"%'u(^42^)"|};
  scan_str {|"%'·u(^42^)"|};
  scan_str {|"%'··u(^42^)"|};

  scan_str {|"%00u(^42^)"|};
  scan_str {|"%008u(^42^)"|};

  scan_str {|"%u064(^42^)"|};
  scan_str {|"%i064(^42i^)"|};
  scan_str {|"%r064(^42.^)"|};
  scan_str {|"%u63(^42^)"|};
  scan_str {|"%i63(^42i^)"|};
  scan_str {|"%r63(^42.^)"|};

  scan_str {|"%u(^42^)|};
  scan_str {|"%u(^42^)>>>|};

  scan_str {|"%|};
  scan_str {|"%'·'|};
  scan_str {|"%8|};
  scan_str {|"%*u(^42^)|};
  scan_str {|"%(^42^)|};
  scan_str {|"%'·'(^42^)|};
  scan_str {|"%^(^42^)|};
  scan_str {|"%+(^42^)|};
  scan_str {|"%#(^42^)|};
  scan_str {|"%8(^42^)|};
  scan_str {|"%.u(^42^)|};
  scan_str {|"%.=u(^42^)|};
  scan_str {|"%.3|};
  scan_str {|"%.*�(^42^)"|};
  scan_str {|"%.3(^42^)"|};
  scan_str {|"%x(^42^)"|};
  scan_str {|"%m(^42^)"|};
  scan_str {|"%p(^42^)"|};
  scan_str {|"%f�(^42^)"|};
  scan_str {|"%u�(^42^)"|};
  scan_str {|"%u=�(^42^)"|};
  scan_str {|"%u (^42^)"|};
  scan_str {|"%u(42^)"|}

let _ = test ()
