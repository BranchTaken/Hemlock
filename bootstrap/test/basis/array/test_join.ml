open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test ?sep arrs = begin
    printf "join";
    let () = match sep with
      | None -> ()
      | Some sep -> printf " ~sep:%a" (pp Uns.pp) sep;
    in
    printf " %a -> %a\n"
      (List.pp (pp Uns.pp)) arrs
      (pp Uns.pp) (join ?sep arrs)
  end in
  printf "@[<h>";
  test [];
  test [[||]];
  test [[||]; [||]];
  test [[||]; [||]; [||]];

  test [[|0|]];

  test [[|0|]; [||]];
  test [[||]; [|0|]];
  test [[|0|]; [|1|]];

  test [[|0|]; [||]; [||]];
  test [[||]; [|0|]; [||]];
  test [[||]; [||]; [|0|]];
  test [[|0|]; [|1|]; [||]];
  test [[|0|]; [||]; [|1|]];
  test [[|0|]; [|1|]; [|2|]];

  test ~sep:[|3|] [];
  test ~sep:[|3|] [[||]];
  test ~sep:[|3|] [[||]; [||]];
  test ~sep:[|3|] [[||]; [||]; [||]];

  test ~sep:[|3|] [[|0|]];

  test ~sep:[|3|] [[|0|]; [||]];
  test ~sep:[|3|] [[||]; [|0|]];
  test ~sep:[|3|] [[|0|]; [|1|]];

  test ~sep:[|3|] [[|0|]; [||]; [||]];
  test ~sep:[|3|] [[||]; [|0|]; [||]];
  test ~sep:[|3|] [[||]; [||]; [|0|]];
  test ~sep:[|3|] [[|0|]; [|1|]; [||]];
  test ~sep:[|3|] [[|0|]; [||]; [|1|]];
  test ~sep:[|3|] [[|0|]; [|1|]; [|2|]];
  printf "@]"

let _ = test ()
