open Core
open Async
open Log.Global

open Blockexplorer
open Blockexplorer_async

let loglevel_of_int = function 2 -> `Info | 3 -> `Debug | _ -> `Error

let fetch_utxos loglevel testnet addrs () =
  set_level (loglevel_of_int loglevel) ;
  debug "Looking for UTXOs" ;
  utxos ~testnet addrs >>= function
  | Error err ->
    error "%s" (Http.string_of_error err) ;
    Shutdown.exit 1
  | Ok utxos ->
    let _ret = List.iter2 addrs utxos ~f:begin fun a u ->
        printf "%s" (Utxo.to_string u)
      end in
    Shutdown.exit 0

let fetch_utxos =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-loglevel" (optional_with_default 1 int) ~doc:"1-3 global loglevel"
    +> flag "-testnet" no_arg ~doc:" Use bitcoin testnet"
    +> anon (sequence ("addr" %: string))
  in
  Command.async ~summary:"Get UTXOs" spec fetch_utxos

let command =
  Command.group ~summary:"blockexplorer.com tools" [
    "fetch-utxos", fetch_utxos ;
  ]

let () = Command.run command
