open Core
open Async

open Blockexplorer
open Blockexplorer.Types

val tx :
  ?testnet:bool -> Hex.t -> Tx.t Http.result Deferred.t

val rawtx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result Deferred.t

val utxos :
  ?testnet:bool -> Base58.t list -> (Utxo.t list) Http.result Deferred.t

val broadcast_tx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result Deferred.t

val tx_by_addr :
  ?testnet:bool -> Base58.t -> Tx.t list Http.result Deferred.t
