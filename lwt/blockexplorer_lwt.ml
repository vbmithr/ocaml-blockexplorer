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

type network =
  | BTC
  | BTC_testnet
  | BCH
  | BCH_testnet

let network_of_string s =
  match String.lowercase s with
  | "btc" | "xbt" -> BTC
  | "tbtc" | "txbt" -> BTC_testnet
  | "bch" | "bcc" -> BCH
  | "tbch" | "btcc" -> BCH_testnet
  | _ -> invalid_arg "network_of_string"

let string_of_network = function
  | BTC -> "btc"
  | BTC_testnet -> "tbtc"
  | BCH -> "bch"
  | BCH_testnet -> "tbch"

let network_pp ppf t =
  Caml.Format.fprintf ppf "%s" (string_of_network t)

let url_of_network = function
  | BTC -> "https://blockexplorer.com/api"
  | BTC_testnet -> "https://testnet.blockexplorer.com/api"
  | BCH -> "https://bcc.blockdozer.com/insight-api"
  | BCH_testnet -> "https://tbcc.blockdozer.com/insight-api"

let url_of_network n = Uri.of_string (url_of_network n)

let uri_append_path uri path =
  let old_path = Uri.path uri in
  Uri.with_path uri (old_path ^ "/" ^ path)

let tx ?(network=BTC) (`Hex txid) =
  let url = url_of_network network in
  let url = uri_append_path url ("tx/" ^ txid) in
  safe_get ~encoding:Tx.encoding url

let rawtx ?(network=BTC) (`Hex txid) =
  let url = url_of_network network in
  let url = uri_append_path url ("rawtx/" ^ txid) in
  let encoding = Json_encoding.(obj1 (req "rawtx" string)) in
  safe_get ~encoding url >>| R.map (fun tx_hex -> Hex.to_string (`Hex tx_hex))

let utxos ?(network=BTC) = function
  | [] -> Lwt.return (Ok [])
  | addrs ->
      let url = url_of_network network in
      let url = uri_append_path url "addrs/utxo" in
      let params = ["addrs", List.map addrs ~f:Base58.Bitcoin.to_string] in
      safe_post ~params ~encoding:Json_encoding.(list Utxo.encoding) url

let broadcast_tx ?(network=BTC) rawtx_bytes =
  let url = url_of_network network in
  let url = uri_append_path url "tx/send" in
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

let tx_by_addr ?(network=BTC) ?(page=0) addr =
  let url = url_of_network network in
  let url = uri_append_path url "txs" in
  let url = Uri.with_query url [
      "address", [ Base58.Bitcoin.to_string addr ] ;
      "pageNum", [ Int.to_string page ] ;
    ] in
  safe_get ~encoding:(tx_by_addr_encoding page) url

let all_tx_by_addr ?(network=BTC) addr =
  let rec loop acc id =
    tx_by_addr ~network ~page:id addr >>= function
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

let tx_by_addrs ?(network=BTC) ?start ?stop addrs =
  let url = url_of_network network in
  let url = uri_append_path url "addrs/txs" in
  let params = List.filter_opt [
      Some ("addrs", List.map addrs ~f:Base58.Bitcoin.to_string) ;
      Option.map start ~f:(fun i -> "from", [Int.to_string i]) ;
      Option.map stop ~f:(fun i -> "to", [Int.to_string i]) ;
    ] in
  safe_post ~params ~encoding:tx_by_addrs_result_encoding url

let all_tx_by_addrs ?(network=BTC) ?(pagesize=50) addrs =
  if pagesize <= 0 || pagesize > 50 then
    invalid_arg "all_tx_by_addrs: pagesize must be between 1 and 50" ;
  let rec loop acc start =
    let stop = start + pagesize in
    tx_by_addrs ~network ~start ~stop addrs >>= function
    | Error _ as e -> Lwt.return e
    | Ok { nb_txs; from_id; to_id; txs } ->
      if to_id = nb_txs then Lwt.return_ok (txs @ acc)
      else loop (txs @ acc) to_id
  in loop [] 0

let network_status ?(network=BTC) () =
  let url = url_of_network network in
  let url = uri_append_path url "status" in
  let url = Uri.with_query url ["q", [ "getInfo" ]] in
  let encoding =
    Json_encoding.(obj1 (req "info" Network_status.encoding)) in
  safe_get ~encoding url

let hash_of_block_index ?(network=BTC) index =
  let url = url_of_network network in
  let url = uri_append_path url ("block-index/" ^ Int.to_string index) in
  let encoding =
    Json_encoding.(obj1 (req "blockHash" string)) in
  safe_get ~encoding url >>|
  R.map (fun blockhash -> `Hex blockhash)

let best_block_hash ?(network=BTC) () =
  let url = url_of_network network in
  let url = uri_append_path url "status" in
  let url = Uri.with_query url ["q", [ "getBestBlockHash" ]] in
  let encoding =
    Json_encoding.(obj1 (req "bestblockhash" string)) in
  safe_get ~encoding url >>| R.map (fun blockhash -> `Hex blockhash)

let rawblock ?(network=BTC) (`Hex blockhash) =
  let url = url_of_network network in
  let url = uri_append_path url ("rawblock/" ^ blockhash) in
  let encoding =
    Json_encoding.(obj1 (req "rawblock" string)) in
  safe_get ~encoding url >>|
  R.map (fun block_hex -> Hex.to_string (`Hex block_hex))

let block ?(network=BTC) (`Hex blockhash) =
  let url = match network with
    | BTC -> "https://webbtc.com"
    | BTC_testnet -> "https://test.webbtc.com"
    | _ -> invalid_arg "Blockexplorer_lwt.block: unsupported network" in
  let url = Uri.with_path (Uri.of_string url) ("block/" ^ blockhash ^ ".json") in
  safe_get ~encoding:Block.encoding url
