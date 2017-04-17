open Core
open Async
open Log.Global

open Blockexplorer
open Blockexplorer_async

let loglevel_of_int = function 2 -> `Info | 3 -> `Debug | _ -> `Error

let fetch_utxos loglevel testnet addrs () =
  set_level (loglevel_of_int loglevel) ;
  stage begin fun `Scheduler_started ->
    utxos ~testnet addrs >>= function
    | Error err ->
        error "%s" (Http.string_of_error err) ;
        Scheduler.yield_until_no_jobs_remain () >>= fun () ->
        Shutdown.exit 1
    | Ok utxos ->
        let _ret = List.iter2 addrs utxos ~f:begin fun a u ->
            printf "%s" (Utxo.to_string u)
          end in
        Scheduler.yield_until_no_jobs_remain ()
  end

let fetch_utxos =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (sequence ("addr" %: string))
  in
  Command.Staged.async
    ~summary:"Get Bitcoin UTXOs from a Tezos public key hash" spec fetch_utxos

let command =
  Command.group ~summary:"blockexplorer.com tools" [
    "fetch-utxos", fetch_utxos ;
  ]

let () = Command.run command
