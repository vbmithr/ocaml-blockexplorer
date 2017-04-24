module Tx = struct
  module ScriptSig = struct
    type t = {
      asm : string ;
      hex : string ;
    }

    let empty = { asm = "" ; hex = "" }

    let encoding =
      let open Json_encoding in
      conv
        (fun { asm ; hex } -> (asm, hex))
        (fun (asm, hex) -> { asm ; hex })
        (obj2
           (req "asm" string)
           (req "hex" string))
  end

  module ScriptPubKey = struct
    type typ =
      | Pubkeyhash

    let typ_of_string = function
      | "pubkeyhash" -> Pubkeyhash
      | _ -> invalid_arg "typ_of_string"

    let typ_to_string = function
      | Pubkeyhash -> "pubkeyhash"

    type t = {
      addresses : Base58.t list ;
      asm : string list ;
      hex : string ;
      typ : typ ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { addresses ; asm ; hex ; typ } ->
           let typ = typ_to_string typ in
           let asm = String.concat " " asm in
           let addresses = ListLabels.map addresses ~f:Base58.to_string in
           (addresses, asm, hex, typ))
        (fun (addresses, asm, hex, typ) ->
           let typ = typ_of_string typ in
           let asm = String.split_on_char ' ' asm in
           let addresses =
             ListLabels.map addresses ~f:Base58.of_string_exn in
           { addresses ; asm ; hex ; typ })
        (obj4
           (req "addresses" (list string))
           (req "hex" string)
           (req "asm" string)
           (req "type" string))
  end

  module Vin = struct
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

    let unsigned_int =
      Json_encoding.ranged_int ~minimum:0 ~maximum:max_int "unsigned int"
    type t = {
      sequence : int ;
      n : int ;
      input : input ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { sequence ; n ; input } -> match input with
           | Coinbase h ->
               (sequence, n, Some h, None, None, None,
                None, None, None, None)
           | Tx { txid ; value ; doubleSpentTxID ; addr = (`Base58 b58);
                  scriptSig ; vout } ->
               (sequence, n, None, Some txid, Some value, None, doubleSpentTxID,
                Some b58, Some scriptSig, Some vout))
        (fun (sequence, n, coinbase, txid, valueSat, value, doubleSpentTxID,
              addr, scriptSig, vout) ->
          match coinbase with
          | Some h -> { sequence ; n ; input = Coinbase h }
          | None ->
              match txid, valueSat, addr, scriptSig, vout with
              | Some txid, Some valueSat, Some addr, Some scriptSig, Some vout ->
              let input = Tx {
                  txid ;
                  value = valueSat ;
                  doubleSpentTxID ;
                  addr = Base58.of_string_exn addr ;
                  scriptSig ;
                  vout } in
              { sequence ; n ; input }
              | _ -> invalid_arg "Tx.encoding")
        (obj10
           (req "sequence" unsigned_int)
           (req "n" int)
           (opt "coinbase" string)
           (opt "txid" string)
           (opt "valueSat" int)
           (opt "value" float)
           (dft "doubleSpentTxID" (option string) None)
           (opt "addr" string)
           (opt "scriptSig" ScriptSig.encoding)
           (opt "vout" int))
  end

  module Vout = struct
    type t = {
      n : int ;
      value : int ;
      spentTxId : string option ;
      spentIndex : int option ;
      spentHeight : int option ;
      scriptPubKey : ScriptPubKey.t ;
    }

    let encoding =
      let open Json_encoding in
      conv
        (fun { n ; value ; spentTxId ; spentIndex ; spentHeight ; scriptPubKey } ->
           let value = string_of_float (float_of_int value /. 1e8) in
           (n, value, spentTxId, spentIndex, spentHeight, scriptPubKey))
        (fun (n, value, spentTxId, spentIndex, spentHeight, scriptPubKey) ->
           let value = int_of_float (float_of_string value *. 1e8) in
           { n ; value ; spentTxId ; spentIndex ; spentHeight ; scriptPubKey })
        (obj6
           (req "n" int)
           (req "value" string)
           (req "spentTxId" (option string))
           (req "spentIndex" (option int))
           (req "spentHeight" (option int))
           (req "scriptPubKey" ScriptPubKey.encoding))
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

  let encoding =
    let open Json_encoding in
    conv
      (fun { txid ; version ; isCoinbase ; size ; time ; locktime ; confirmations ;
             valueIn ; valueOut ; fees ;
             blockheight ; blocktime ; blockhash ; vin ; vout } ->
        let time = Ptime.to_float_s time in
        let valueIn = float_of_int valueIn /. 1e8 in
        let valueOut = float_of_int valueOut /. 1e8 in
        let fees = float_of_int fees /. 1e8 in
        let blocktime = Base.Option.map blocktime
            ~f:(fun t -> Ptime.to_float_s t) in
        ((blockheight, blocktime, blockhash, vin, vout),
         (txid, version, isCoinbase, size, time, locktime, confirmations,
          valueIn, valueOut, fees)))
      (fun ((blockheight, blocktime, blockhash, vin, vout),
            (txid, version, isCoinbase, size, time, locktime,
             confirmations, valueIn, valueOut, fees)) ->
        let time =
          Base.Option.value_exn (Ptime.of_float_s time) in
        let blocktime = Base.Option.bind blocktime
            ~f:(fun t -> Ptime.of_float_s t) in
        let valueIn = int_of_float (valueIn *. 1e8) in
        let valueOut = int_of_float (valueOut *. 1e8) in
        let fees = int_of_float (fees *. 1e8) in
        { txid ; version ; isCoinbase ; size ; time ; locktime ; confirmations ;
             valueIn ; valueOut ; fees ;
             blockheight ; blocktime ; blockhash ; vin ; vout })
      (merge_objs
         (obj5
            (req "blockheight" int)
            (opt "blocktime" float)
            (opt "blockhash" string)
            (req "vin" (list Vin.encoding))
            (req "vout" (list Vout.encoding)))
         (obj10
            (req "txid" string)
            (req "version" int)
            (dft "isCoinBase" bool false)
            (req "size" int)
            (req "time" float)
            (req "locktime" int)
            (req "confirmations" int)
            (dft "valueIn" float 0.)
            (req "valueOut" float)
            (dft "fees" float 0.)))
end

module Utxo = struct
  type confirmed =
    | Unconfirmed of Ptime.t
    | Confirmed of {
        confirmations : int ;
        vout : int ;
        scriptPubKey : string ;
        height: int ;
      }

  let pp_confirmed ppf = function
    | Unconfirmed ts ->
      Format.fprintf ppf "Unconfirmed %a" Ptime.pp ts
    | Confirmed { confirmations ; vout ; scriptPubKey ; height } ->
      Format.fprintf ppf
        "Confirmed {@[<hov 1> confirmations = %d ;@;vout = %d;@;scriptPubKey = %s ;@;height = %d }@]"
        confirmations vout scriptPubKey height

  type t = {
    address : Base58.t ;
    txid : string ;
    amount : int ; (* in sats *)
    confirmed: confirmed ;
  }

  let pp ppf { address = `Base58 b58 ; txid ; amount ; confirmed } =
    Format.fprintf ppf
      "{@[<hov 1> address = %s ;@;txid = %s ;@;amount = %d ;@;confirmed = %a }@]"
      b58 txid amount pp_confirmed confirmed

  let encoding =
    let open Json_encoding in
    conv
      (fun { address = `Base58 b58 ; txid ; amount ; confirmed } ->
         let sats = float_of_int amount in
         let amount = sats /. 1e8 in
         ( b58, txid, None, 0., None, amount, sats, None, 0, None))
      (fun (address, txid, vout, ts, scriptPubKey, _amount, sats, height, confirmations, _) ->
         let address = Base58.of_string_exn address in
         let ts = match Ptime.of_float_s ts with
           | None -> invalid_arg "Ptime.of_float_s"
           | Some ts -> ts in
         let amount = int_of_float sats in
         let confirmed = match confirmations with
         | 0 -> Unconfirmed ts
         | n ->
           let vout, scriptPubKey, height = match vout, scriptPubKey, height with
             | Some v, Some s, Some h -> v, s, h
             | _ -> failwith "Utxo.encoding" in
           Confirmed { confirmations ; vout ; scriptPubKey ; height } in
         { address ; txid ; amount ; confirmed })
      (obj10
         (req "address" string)
         (req "txid" string)
         (opt "vout" int)
         (dft "ts" float 0.)
         (opt "scriptPubKey" string)
         (req "amount" float)
         (req "satoshis" float)
         (opt "height" int)
         (req "confirmations" int)
         (opt "confirmationsFromCache" bool))

  let to_string t =
    Format.asprintf "%a" pp t
end
