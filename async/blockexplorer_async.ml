open Core
open Async
open Cohttp_async

open Blockexplorer
open Blockexplorer.Types

exception Client of string
exception Server of string
exception API of string
exception Data_encoding of Ezjsonm.t

let safe_get ?headers ~encoding url =
  Monitor.try_with ~extract_exn:true begin fun () ->
    Client.get ?headers url >>= fun (resp, body) ->
    let status_code = Cohttp.Code.code_of_status resp.status in
    Body.to_string body >>| fun body_str ->
    if Cohttp.Code.is_client_error status_code then
      raise (Client body_str)
    else if Cohttp.Code.is_server_error status_code then
      raise (Server body_str) ;
    let json = Ezjsonm.from_string body_str in
    try
      Json_encoding.destruct encoding json
    with _ -> raise (Data_encoding json)
  end >>| Result.map_error ~f:begin function
    | Client str -> Http.Client str
    | Server str -> Server str
    | API str -> API str
    | Data_encoding json -> Data_encoding json
    | exn -> Cohttp exn
  end

let safe_post ?headers ~params ~encoding url =
  Monitor.try_with ~extract_exn:true begin fun () ->
    Client.post_form ?headers ~params url >>= fun (resp, body) ->
    let status_code = Cohttp.Code.code_of_status resp.status in
    Body.to_string body >>| fun body_str ->
    if Cohttp.Code.is_client_error status_code then raise (Client body_str)
    else if Cohttp.Code.is_server_error status_code then raise (Server body_str) ;
    let json = Ezjsonm.from_string body_str in
    try
      Json_encoding.destruct encoding json
    with _ -> raise (Data_encoding json)
  end >>| Result.map_error ~f:begin function
    | Client str -> Http.Client str
    | Server str -> Http.Server str
    | API str -> Http.API str
    | Data_encoding json -> Http.Data_encoding json
    | exn -> Http.Cohttp exn
  end

let base_url = Uri.of_string "https://blockexplorer.com"
let testnet_base_url = Uri.of_string "https://testnet.blockexplorer.com"

let tx ?(testnet=false) (`Hex txid) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url (Printf.sprintf "/api/tx/%s" txid) in
  safe_get ~encoding:Tx.encoding url

let rawtx ?(testnet=false) (`Hex txid) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url (Printf.sprintf "/api/rawtx/%s" txid) in
  let encoding = Json_encoding.(obj1 (req "rawtx" string)) in
  safe_get ~encoding url >>| Result.map ~f:(fun txid -> `Hex txid)

let utxos ?(testnet=false) = function
  | [] -> Deferred.return (Ok [])
  | addrs ->
      let url = if testnet then testnet_base_url else base_url in
      let url = Uri.with_path url "/api/addrs/utxo" in
      let params = ["addrs", addrs] in
      safe_post ~params ~encoding:Json_encoding.(list Utxo.encoding) url

let broadcast_tx ?(testnet=false) (`Hex rawtx_hex) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/tx/send" in
  let params = ["rawtx", [rawtx_hex]] in
  let encoding = Json_encoding.(obj1 (req "txid" string)) in
  safe_post ~params ~encoding url >>| Result.map ~f:(fun txid -> `Hex txid)

let tx_by_addr ?(testnet=false) addr =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/txs" in
  let url = Uri.with_query url ["address", [addr]] in
  let encoding =
    let open Json_encoding in
    conv (fun s -> (0, s)) (fun (_, s) -> s)
      (obj2 (req "pagesTotal" int) (req "txs" (list Tx.encoding))) in
  safe_get ~encoding url
