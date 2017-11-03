open Blockexplorer
open Types

type network =
  | BTC
  | BTC_testnet
  | BCH
  | BCH_testnet

val network_of_string : string -> network
val network_pp : Format.formatter -> network -> unit

val tx :
  ?network:network -> Hex.t -> Tx.t Http.result Lwt.t
(** [tx ?testnet txid] returns [Ok tx], where tx is the parsed
    representation of transaction id [txid], or an error. *)

val rawtx :
  ?network:network -> Hex.t -> string Http.result Lwt.t
(** [rawtx ?testnet txid] returns [Ok bytes], where bytes is the binary
    representation of transaction id [txid], or an error. *)

val utxos :
  ?network:network -> Base58.Bitcoin.t list -> (Utxo.t list) Http.result Lwt.t

val broadcast_tx :
  ?network:network -> string -> Hex.t Http.result Lwt.t
(** [broadcast_tx ?testnet tx_bytes] returns [Ok txid], where [txid]
    is the id of successfully broadcasted transaction with binary
    representation [tx_bytes], or an error. *)

type tx_by_addr_result = {
  page_id : int ;
  nb_pages : int ;
  txs : Tx.t list ;
}

val tx_by_addr :
  ?network:network -> ?page:int -> Base58.Bitcoin.t ->
  tx_by_addr_result Http.result Lwt.t

val all_tx_by_addr :
  ?network:network -> Base58.Bitcoin.t ->
  Tx.t list Http.result Lwt.t

type tx_by_addrs_result = {
  nb_txs : int ;
  from_id : int ;
  to_id : int ;
  txs : Tx.t list ;
}

val tx_by_addrs :
  ?network:network -> ?start:int -> ?stop:int -> Base58.Bitcoin.t list ->
  tx_by_addrs_result Http.result Lwt.t

val all_tx_by_addrs :
  ?network:network -> ?pagesize:int -> Base58.Bitcoin.t list ->
  Tx.t list Http.result Lwt.t

val network_status : ?network:network -> unit -> Network_status.t Http.result Lwt.t

val hash_of_block_index : ?network:network -> int -> Hex.t Http.result Lwt.t

val best_block_hash : ?network:network -> unit -> Hex.t Http.result Lwt.t

val rawblock : ?network:network -> Hex.t -> string Http.result Lwt.t
(** [rawblock ?testnet blockhash] returns [Ok bytes], where bytes is
    the binary representation of block hash [blockhash], or an
    error. *)

val block : ?network:network -> Hex.t -> Block.t Http.result Lwt.t
