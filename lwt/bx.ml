open Cmdliner
open Lwt.Infix
open Blockexplorer
open Blockexplorer.Types
open Blockexplorer_lwt

let fetch_utxos loglevel testnet addrs =
  let run () =
    Lwt_log.debug "Looking for UTXOs" >>= fun () ->
    utxos ~testnet addrs >>= function
    | Error err ->
      Lwt_log.error (Http.string_of_error err) >|= fun () ->
      exit 1
    | Ok utxos ->
      ListLabels.iter utxos ~f:begin fun u ->
        Lwt_log.ign_notice (Utxo.to_string u)
      end ;
      Lwt.return_unit
  in Lwt_main.run (run ())

let loglevel =
  let doc = "Print more debug messages. Can be repeated." in
  Arg.(value & flag_all & info ["v"] ~doc)

let testnet =
  let doc = "Use Bitcoin testnet." in
  Arg.(value & flag & info ["t" ; "testnet"] ~doc)

let fetch_utxos =
  let payment_addr =
    (fun str ->
       match Base58.Bitcoin.of_string str with
       | Some addr -> `Ok addr
       | None -> `Error (Printf.sprintf "Bitcoin multisig address expected, got %s" str)),
    Base58.Bitcoin.pp in
  let addrs =
    Arg.(non_empty & (pos_all payment_addr []) & info [] ~docv:"ADDR") in
  Term.(const fetch_utxos $ loglevel $ testnet $ addrs),
  Term.info "fetch-utxos"

let default_cmd =
  let doc = "Cmdline utility for Blockexplorer.com" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "bx" ~doc

let cmds = [fetch_utxos]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | #Term.result -> exit 0



