open! Basis.Rudiments
open! Basis
open! Hmc
open! ScanTest

let test () =
  scan_str "";
  scan_str "a";
  scan_str "\n";
  scan_str "    a";
  scan_str {|    a
|};
  scan_str {|    a
b
|};
  scan_str {|a
b
c
    d
    e
    f
g
h
i
|};
  scan_str {|a
    b
      c
    d
e
|};
  scan_str {|
a
b
    c
d
|};
  scan_str {|
a
    b
  c
    d
|};
  scan_str {|  a
  b
      c
      d
|};
  scan_str {|a
    b
    c

    d
        e
        f|};
  scan_str {|a
    \
b
    c|};
  scan_str {|\
    a
    \
b
    c \
    d|};
  scan_str {|\
    a\
    b \
    c
    d|};
  scan_str "\t|";
  scan_str "  ";
  scan_str "\n  \n";
  scan_str "\\\n";
  scan_str "\\\na";
  scan_str "\\\n\\\na";
  scan_str {|# a
b
    c
    # d
# e
    f
g
|};
  scan_str "a\n  \n  \nb";
  scan_str {|a
    b
(* Ignore. *)
    c
|};
  scan_str {|a
    b
(* Ignore. *) (* ... *) # ...
    c
 (* Ignore. *) (* ... *) # ...
    d
  (* Ignore. *) (* ... *) # ...
    e
   (* Ignore. *) (* ... *) # ...
    f
    (* Ignore. *) (* ... *) # ...
    g
     (* Ignore. *) (* ... *) # ...
        h
            (* Ignore. *) (* ... *) # ...
        i
|};
  scan_str {|a
    b
(* Ignore. *) (* ... *) # ...
    c
|};
  scan_str {|a
    b
(* Ignore. *) (*
 ... *) \
# ...
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) true
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) (* ...
 *) \
 true
    c
|};
  scan_str {|a
    b
(* Don't ignore. *) (*
 ... *) true
    c
|};
  scan_str {|a
    b
        c
            d
    e
|};
  scan_str {|a
    b
        c
            d
      e
    f
|};
  scan_str {|a
    b
    c
  d
    e
        f
      g
        h
    i
|};
  scan_str {|a
  b
  c
d
|};
  scan_str {|a
        b
|};
  scan_str {|a
    b
        c
d
|};

  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
a2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
 x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
  a'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
   x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
    b2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
     x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
      b'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
       x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
        c2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
         x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
          c'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
           x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
            d2
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
             x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
              d'
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
               x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                e
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                 x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                  x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                   x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                    x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                     x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                      x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                       x
|};
  scan_str {|
# : | : | : | : | : | : |
a
    b
        c
            d
                        x
|};
  scan_str {|
# : | : | : | : | : | : |
        a
    b
            c
                        d
|};

  (* EOI. *)
  scan_str {|(|};
  scan_str {|    |};
  scan_str {|\|}

let _ = test ()
