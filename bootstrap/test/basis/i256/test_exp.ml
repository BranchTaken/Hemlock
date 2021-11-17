open! Basis.Rudiments
open! Basis
open I256

let test () =
  let rec test_pairs = function
    | [] -> ()
    | (x, y) :: pairs' -> begin
        File.Fmt.stdout
        |> pp x
        |> Fmt.fmt " ** "
        |> pp y
        |> Fmt.fmt " -> "
        |> pp (x ** y)
        |> Fmt.fmt "\n"
        |> ignore;
        test_pairs pairs'
      end
  in
  let pairs = [
    (of_string "0", of_string "0");
    (of_string "0", of_string "1");
    (of_string "1", of_string "-1");

    (max_value, of_string "0");
    (max_value, of_string "1");
    (max_value, max_value);
    (max_value, of_string "-254");
    (max_value, of_string "-255");
    (max_value, of_string "-256");

    (min_value, one);
    (min_value, neg_one);

    (of_string "1", max_value);
    (of_string "-1", max_value);

    (of_string "1", min_value);
    (of_string "-1", min_value);

    (of_string "2", of_string "31");
    (of_string "2", of_string "32");
    (of_string "2", of_string "63");
    (of_string "2", of_string "64");
    (of_string "2", of_string "127");
    (of_string "2", of_string "128");
    (of_string "2", of_string "255");
    (of_string "2", of_string "256");
    (of_string "2", of_string "257");

    (of_string "-2", of_string "31");
    (of_string "-2", of_string "32");
    (of_string "-2", of_string "63");
    (of_string "-2", of_string "64");
    (of_string "-2", of_string "127");
    (of_string "-2", of_string "128");
    (of_string "-2", of_string "255");
    (of_string "-2", of_string "256");
    (of_string "-2", of_string "257");
  ] in
  test_pairs pairs

let _ = test ()
