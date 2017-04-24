open Blockexplorer
open Types

val tx :
  ?testnet:bool -> Hex.t -> Tx.t Http.result Lwt.t

val rawtx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result Lwt.t

val utxos :
  ?testnet:bool -> Base58.t list -> (Utxo.t list) Http.result Lwt.t

val broadcast_tx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result Lwt.t

val tx_by_addr :
  ?testnet:bool -> Base58.t -> Tx.t list Http.result Lwt.t
