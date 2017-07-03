open Base

open Rresult
open Lwt.Infix
open Cohttp_lwt_unix
module Body = Cohttp_lwt_body

open Blockexplorer
open Blockexplorer.Types

let (>>|) = Lwt.(>|=)

exception Client of string
exception Server of string
exception API of string
exception Data_encoding of string

let safe_get ?headers ~encoding url =
  Lwt.catch
    begin fun () ->
      Client.get ?headers url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >>| fun body_str ->
      if Cohttp.Code.is_client_error status_code then
        raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then
        raise (Server body_str) ;
      let json = Ezjsonm.from_string body_str in
      try
        R.return (Json_encoding.destruct encoding json)
      with exn ->
        let pp = Json_encoding.print_error ?print_unknown:None in
        let str = Caml.Format.asprintf "%a" pp exn in
        raise (Data_encoding str)

    end
    begin function
      | Client str -> Lwt.return (R.fail (Http.Client str))
      | Server str -> Lwt.return (R.fail (Http.Server str))
      | API str -> Lwt.return (R.fail (Http.API str))
      | Data_encoding msg -> Lwt.return (R.fail (Http.Data_encoding msg))
      | exn -> Lwt.return (R.fail (Http.Cohttp exn))
    end

let safe_post ?headers ~params ~encoding url =
  Lwt.catch
    begin fun () ->
      Client.post_form ?headers ~params url >>= fun (resp, body) ->
      let status_code = Cohttp.Code.code_of_status resp.status in
      Body.to_string body >>| fun body_str ->
      if Cohttp.Code.is_client_error status_code then raise (Client body_str)
      else if Cohttp.Code.is_server_error status_code then raise (Server body_str) ;
      let json = Ezjsonm.from_string body_str in
      try
        R.return (Json_encoding.destruct encoding json)
      with exn ->
        let pp = Json_encoding.print_error ?print_unknown:None in
        let str = Caml.Format.asprintf "%a" pp exn in
        raise (Data_encoding str)
    end
    begin function
      | Client str -> Lwt.return (R.fail (Http.Client str))
      | Server str -> Lwt.return (R.fail (Http.Server str))
      | API str -> Lwt.return (R.fail (Http.API str))
      | Data_encoding msg -> Lwt.return (R.fail (Http.Data_encoding msg))
      | exn -> Lwt.return (R.fail (Http.Cohttp exn))
    end

let base_url = Uri.of_string "https://blockexplorer.com"
let testnet_base_url = Uri.of_string "https://testnet.blockexplorer.com"

let tx ?(testnet=false) (`Hex txid) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url ("/api/tx/" ^ txid) in
  safe_get ~encoding:Tx.encoding url

let rawtx ?(testnet=false) (`Hex txid) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url ("/api/rawtx/" ^ txid) in
  let encoding = Json_encoding.(obj1 (req "rawtx" string)) in
  safe_get ~encoding url >>| R.map (fun tx_hex -> Hex.to_string (`Hex tx_hex))

let utxos ?(testnet=false) = function
  | [] -> Lwt.return (Ok [])
  | addrs ->
      let url = if testnet then testnet_base_url else base_url in
      let url = Uri.with_path url "/api/addrs/utxo" in
      let params = ["addrs", List.map addrs ~f:Base58.Bitcoin.to_string] in
      safe_post ~params ~encoding:Json_encoding.(list Utxo.encoding) url

let broadcast_tx ?(testnet=false) rawtx_bytes =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/tx/send" in
  let `Hex rawtx_hex = Hex.of_string rawtx_bytes in
  let params = ["rawtx", [rawtx_hex]] in
  let encoding = Json_encoding.(obj1 (req "txid" string)) in
  safe_post ~params ~encoding url >>| R.map (fun txid -> `Hex txid)

type tx_by_addr_result = {
  page_id : int ;
  nb_pages : int ;
  txs : Tx.t list ;
}

let tx_by_addr_encoding page_id =
  let open Json_encoding in
  conv
    (fun { page_id ; nb_pages ; txs } -> (nb_pages, txs))
    (fun (nb_pages, txs) -> { page_id ; nb_pages ; txs })
    (obj2
       (req "pagesTotal" int)
       (req "txs" (list Tx.encoding)))

let tx_by_addr ?(testnet=false) ?(page=0) addr =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/txs" in
  let url = Uri.with_query url [
      "address", [ Base58.Bitcoin.to_string addr ] ;
      "pageNum", [ Int.to_string page ] ;
    ] in
  safe_get ~encoding:(tx_by_addr_encoding page) url

let all_tx_by_addr ?(testnet=false) addr =
  let rec loop acc id =
    tx_by_addr ~testnet ~page:id addr >>= function
    | Error _ as e -> Lwt.return e
    | Ok { page_id; nb_pages; txs } ->
      if page_id < nb_pages - 1 then
        loop (txs @ acc) (id + 1)
      else Lwt.return_ok (txs @ acc)
  in
  loop [] 0

type tx_by_addrs_result = {
  nb_txs : int ;
  from_id : int ;
  to_id : int ;
  txs : Tx.t list ;
}

let tx_by_addrs_result_encoding =
  let open Json_encoding in
  conv
    (fun { nb_txs ; from_id ; to_id ; txs } ->
       (nb_txs, from_id, to_id, txs))
    (fun (nb_txs, from_id, to_id, txs) ->
       { nb_txs ; from_id ; to_id ; txs })
    (obj4
       (req "totalItems" int)
       (req "from" int)
       (req "to" int)
       (req "items" (list Tx.encoding)))

let tx_by_addrs ?(testnet=false) ?start ?stop addrs =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/addrs/txs" in
  let params = List.filter_opt [
      Some ("addrs", List.map addrs ~f:Base58.Bitcoin.to_string) ;
      Option.map start ~f:(fun i -> "from", [Int.to_string i]) ;
      Option.map stop ~f:(fun i -> "to", [Int.to_string i]) ;
    ] in
  safe_post ~params ~encoding:tx_by_addrs_result_encoding url

let all_tx_by_addrs ?(testnet=false) ?(pagesize=50) addrs =
  if pagesize <= 0 || pagesize > 50 then
    invalid_arg "all_tx_by_addrs: pagesize must be between 1 and 50" ;
  let rec loop acc start =
    let stop = start + pagesize in
    tx_by_addrs ~testnet ~start ~stop addrs >>= function
    | Error _ as e -> Lwt.return e
    | Ok { nb_txs; from_id; to_id; txs } ->
      if to_id = nb_txs then Lwt.return_ok (txs @ acc)
      else loop (txs @ acc) to_id
  in loop [] 0

let network_status ?(testnet=false) () =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/status" in
  let url = Uri.with_query url ["q", [ "getInfo" ]] in
  let encoding =
    Json_encoding.(obj1 (req "info" Network_status.encoding)) in
  safe_get ~encoding url

let hash_of_block_index ?(testnet=false) index =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url ("/api/block-index/" ^ Int.to_string index) in
  let encoding =
    Json_encoding.(obj1 (req "blockHash" string)) in
  safe_get ~encoding url >>|
  R.map (fun blockhash -> `Hex blockhash)

let best_block_hash ?(testnet=false) () =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url "/api/status" in
  let url = Uri.with_query url ["q", [ "getBestBlockHash" ]] in
  let encoding =
    Json_encoding.(obj1 (req "bestblockhash" string)) in
  safe_get ~encoding url >>| R.map (fun blockhash -> `Hex blockhash)

let rawblock ?(testnet=false) (`Hex blockhash) =
  let url = if testnet then testnet_base_url else base_url in
  let url = Uri.with_path url ("/api/rawblock/" ^ blockhash) in
  let encoding =
    Json_encoding.(obj1 (req "rawblock" string)) in
  safe_get ~encoding url >>|
  R.map (fun block_hex -> Hex.to_string (`Hex block_hex))

let block ?(testnet=false) (`Hex blockhash) =
  let url = Uri.of_string
      (if testnet then "https://test.webbtc.com" else "https://webbtc.com") in
  let url = Uri.with_path url ("block/" ^ blockhash ^ ".json") in
  safe_get ~encoding:Block.encoding url
