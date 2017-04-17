open Core
open Async

open Blockexplorer

module Http : sig
  type error =
    | Cohttp of exn
    | Client of string
    | Server of string
    | API of string
    | Data_encoding of Ezjsonm.t

  val string_of_error : error -> string
  val pp_error : Format.formatter -> error -> unit

  type 'a result = ('a, error) Result.t Deferred.t
end

val tx :
  ?testnet:bool -> Hex.t -> Tx.t Http.result

val rawtx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result

val utxos :
  ?testnet:bool -> string list -> (Utxo.t list) Http.result

val broadcast_tx :
  ?testnet:bool -> Hex.t -> Hex.t Http.result

val tx_by_addr :
  ?testnet:bool -> string -> Tx.t list Http.result
