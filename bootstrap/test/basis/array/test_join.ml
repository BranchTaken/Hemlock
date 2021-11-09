open! Basis.Rudiments
open! Basis
open Array
open Format

let test () =
  let test ?sep arrs = begin
    printf "join";
    let () = match sep with
      | None -> ()
      | Some sep -> printf " ~sep:%a" (xpp Uns.xpp) sep;
    in
    printf " %a -> %a\n"
      (List.xpp (xpp Uns.xpp)) arrs
      (xpp Uns.xpp) (join ?sep arrs)
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
