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

  test [[|0L|]];

  test [[|0L|]; [||]];
  test [[||]; [|0L|]];
  test [[|0L|]; [|1L|]];

  test [[|0L|]; [||]; [||]];
  test [[||]; [|0L|]; [||]];
  test [[||]; [||]; [|0L|]];
  test [[|0L|]; [|1L|]; [||]];
  test [[|0L|]; [||]; [|1L|]];
  test [[|0L|]; [|1L|]; [|2L|]];

  test ~sep:[|3L|] [];
  test ~sep:[|3L|] [[||]];
  test ~sep:[|3L|] [[||]; [||]];
  test ~sep:[|3L|] [[||]; [||]; [||]];

  test ~sep:[|3L|] [[|0L|]];

  test ~sep:[|3L|] [[|0L|]; [||]];
  test ~sep:[|3L|] [[||]; [|0L|]];
  test ~sep:[|3L|] [[|0L|]; [|1L|]];

  test ~sep:[|3L|] [[|0L|]; [||]; [||]];
  test ~sep:[|3L|] [[||]; [|0L|]; [||]];
  test ~sep:[|3L|] [[||]; [||]; [|0L|]];
  test ~sep:[|3L|] [[|0L|]; [|1L|]; [||]];
  test ~sep:[|3L|] [[|0L|]; [||]; [|1L|]];
  test ~sep:[|3L|] [[|0L|]; [|1L|]; [|2L|]];
  printf "@]"

let _ = test ()
