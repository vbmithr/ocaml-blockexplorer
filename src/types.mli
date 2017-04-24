module Tx : sig
  module ScriptSig : sig
    type t = {
      asm : string ;
      hex : string ;
    }

    val encoding : t Json_encoding.encoding
  end

  module ScriptPubKey : sig
    type typ =
      | Pubkeyhash

    type t = {
      addresses : Base58.t list ;
      asm : string list ;
      hex : string ;
      typ : typ ;
    }

    val encoding : t Json_encoding.encoding
  end

  module Vin : sig
    type input =
      | Coinbase of string
      | Tx of {
          txid : string ;
          value: int ;
          doubleSpentTxID: string option ;
          addr : Base58.t ;
          scriptSig : ScriptSig.t ;
          vout : int ;
        }
    type t = {
      sequence : int ;
      n : int ;
      input : input ;
    }

    val encoding : t Json_encoding.encoding
  end

  module Vout : sig
    type t = {
      n : int ;
      value : int ;
      spentTxId : string option ;
      spentIndex : int option ;
      spentHeight : int option ;
      scriptPubKey : ScriptPubKey.t ;
    }

    val encoding : t Json_encoding.encoding
  end

  type t = {
    txid : string ;
    version : int ;
    isCoinbase : bool ;
    size : int ;
    time : Ptime.t ;
    locktime : int ; (* in blocks *)
    confirmations : int ;
    valueIn : int ;
    valueOut : int ;
    fees : int ;

    blockheight : int ;
    blocktime : Ptime.t option ;
    blockhash : string option ;

    vin: Vin.t list ;
    vout : Vout.t list ;
  }

  val encoding : t Json_encoding.encoding
end

module Utxo : sig
  type confirmed =
    | Unconfirmed of Ptime.t
    | Confirmed of {
        confirmations : int ;
        vout : int ;
        scriptPubKey : string ;
        height: int ;
      }

  type t = {
    address : Base58.t ;
    txid : string ;
    amount : int ; (* in sats *)
    confirmed: confirmed ;
  }

  val encoding : t Json_encoding.encoding

  val pp : Format.formatter -> t -> unit
  val to_string : t -> string
end
