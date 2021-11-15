open! Basis.Rudiments
open! Basis
open Array

let test () =
  let test ?sep arrs = begin
    File.Fmt.stdout
    |> Fmt.fmt "join"
    |> (fun formatter ->
      match sep with
      | None -> formatter
      | Some sep -> formatter |> Fmt.fmt " ~sep:" |> (pp Uns.pp) sep
    )
    |> Fmt.fmt " "
    |> (List.pp (pp Uns.pp)) arrs
    |> Fmt.fmt " -> "
    |> (pp Uns.pp) (join ?sep arrs)
    |> Fmt.fmt "\n"
    |> ignore
  end in
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
  test ~sep:[|3L|] [[|0L|]; [|1L|]; [|2L|]]

let _ = test ()
