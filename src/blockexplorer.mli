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
      addresses : string list ;
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
          addr : string ;
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
  type t = {
    address : string ;
    txid : string ;
    vout : int ;
    ts : Ptime.t ;
    scriptPubKey : string ;
    amount : int ; (* in sats *)
    height: int ;
    confirmations : int ;
  }

  val encoding : t Json_encoding.encoding
  val to_json : t -> Json_repr.ezjsonm
  val to_string : t -> string
  val pp : Format.formatter -> t -> unit
end
