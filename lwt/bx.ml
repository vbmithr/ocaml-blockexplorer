open Cmdliner
open Lwt.Infix
open Blockexplorer
open Blockexplorer.Types
open Blockexplorer_lwt

let loglevel =
  let doc = "Print more debug messages. Can be repeated." in
  Arg.(value & flag_all & info ["v"] ~doc)

let testnet =
  let doc = "Use Bitcoin testnet." in
  Arg.(value & flag & info ["t" ; "testnet"] ~doc)

let hex =
  (fun str ->
     try `Ok (Hex.to_string (`Hex str))
     with _ ->`Error (Printf.sprintf "Hex expected, got %s" str)),
  (fun ppf hex ->
     let `Hex hex_str = Hex.of_string hex in
     Format.fprintf ppf "%s" hex_str)

let fetch_utxos loglevel testnet addrs =
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

let fetch_utxos loglevel testnet addrs =
  Lwt_main.run (fetch_utxos loglevel testnet addrs)

let fetch_utxos =
  let payment_addr =
    (fun str ->
       match Base58.Bitcoin.of_string str with
       | Some addr -> `Ok addr
       | None -> `Error (Printf.sprintf "Bitcoin multisig address expected, got %s" str)),
    Base58.Bitcoin.pp in
  let addrs =
    Arg.(non_empty & (pos_all payment_addr []) & info [] ~docv:"ADDR") in
  let doc = "Fetch UTXOs." in
  Term.(const fetch_utxos $ loglevel $ testnet $ addrs),
  Term.info "fetch-utxos" ~doc

let fetch_rawblock loglevel testnet blockhash dest =
  begin rawblock ~testnet (Hex.of_string blockhash) >>= function
    | Error err -> Lwt_log.error (Http.string_of_error err)
    | Ok rawblock ->
      let open Lwt_io in
      with_file ~mode:Output dest (fun oc -> write oc rawblock)
  end

let fetch_rawblock loglevel testnet blockhash dest =
  Lwt_main.run (fetch_rawblock loglevel testnet blockhash dest)

let fetch_rawblock =
  let blockhash =
    Arg.(required & (pos 0 (some hex) None) & info [] ~docv:"BLOCK_HASH") in
  let dest =
    Arg.(required & (pos 1 (some string) None) & info [] ~docv:"FILE") in
  let doc = "Fetch raw block." in
  Term.(const fetch_rawblock $ loglevel $ testnet $ blockhash $ dest),
  Term.info "fetch-rawblock" ~doc

let default_cmd =
  let doc = "Cmdline utility for Blockexplorer.com" in
  Term.(ret (const (`Help (`Pager, None)))),
  Term.info "bx" ~doc

let cmds = [
  fetch_utxos ;
  fetch_rawblock ;
]

let () = match Term.eval_choice default_cmd cmds with
  | `Error _ -> exit 1
  | #Term.result -> exit 0



