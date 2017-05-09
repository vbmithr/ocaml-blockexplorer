open Blockexplorer
open Types

val tx :
  ?testnet:bool -> Hex.t -> Tx.t Http.result Lwt.t
(** [tx ?testnet txid] returns [Ok tx], where tx is the parsed
    representation of transaction id [txid], or an error. *)

val rawtx :
  ?testnet:bool -> Hex.t -> string Http.result Lwt.t
(** [rawtx ?testnet txid] returns [Ok bytes], where bytes is the binary
    representation of transaction id [txid], or an error. *)

val utxos :
  ?testnet:bool -> Base58.Bitcoin.t list -> (Utxo.t list) Http.result Lwt.t

val broadcast_tx :
  ?testnet:bool -> string -> Hex.t Http.result Lwt.t
(** [broadcast_tx ?testnet tx_bytes] returns [Ok txid], where [txid]
    is the id of successfully broadcasted transaction with binary
    representation [tx_bytes], or an error. *)

val tx_by_addr :
  ?testnet:bool -> Base58.Bitcoin.t -> Tx.t list Http.result Lwt.t
