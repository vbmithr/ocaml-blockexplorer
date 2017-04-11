open Core
open Async
open Log.Global

open Blockexplorer
module BA = Blockexplorer_async

let txs = [
  {|{"txid":"5756ff16e2b9f881cd15b8a7e478b4899965f87f553b6210d0f8e5bf5be7df1d","version":1,"locktime":981825022,"vin":[{"coinbase":"03a6ab05e4b883e5bda9e7a59ee4bb99e9b1bc76a3a2bb0e9c92f06e4a6349de9ccc8fbe0fad11133ed73c78ee12876334c13c02000000f09f909f2f4249503130302f4d696e65642062792073647a686162636400000000000000000000000000000000","sequence":2765846367,"n":0}],"vout":[{"value":"25.37726812","n":0,"scriptPubKey":{"hex":"76a914c825a1ecf2a6830c4401620c3a16f1995057c2ab88ac","asm":"OP_DUP OP_HASH160 c825a1ecf2a6830c4401620c3a16f1995057c2ab OP_EQUALVERIFY OP_CHECKSIG","addresses":["1KFHE7w8BhaENAswwryaoccDb6qcT6DbYY"],"type":"pubkeyhash"},"spentTxId":"a01b32246795ca47ed77ef78d56736677ec2f2aae7b400ebbcc95cd784492dc2","spentIndex":6,"spentHeight":371831}],"blockhash":"0000000000000000027d0985fef71cbc05a5ee5cdbdc4c6baf2307e6c5db8591","blockheight":371622,"confirmations":89812,"time":1440604784,"blocktime":1440604784,"isCoinBase":true,"valueOut":25.37726812,"size":185}|} ;
  {|{"txid":"a43ebfaf3a8f151df0e5996e4c4585a70b77ae64f3b2ea202e7a440fdd288354","version":1,"locktime":0,"vin":[{"txid":"1cf0ffab204752dc0b95e91f96fb3db9a4deccded523b81d8c3e5d6356ff7e02","vout":1,"scriptSig":{"asm":"304402205effbbb72110d5d1f818291b9216368a30d99d4ca8ab4a5242a713bb31c1c8a5022025e98dd2275a5a040b99ab0edd1e529d225419ee30fdb3620e81122825debb09[ALL] 020fe041b72b5d713722246f488f4c671f12411b839366c5c8a8589d1432722fdb","hex":"47304402205effbbb72110d5d1f818291b9216368a30d99d4ca8ab4a5242a713bb31c1c8a5022025e98dd2275a5a040b99ab0edd1e529d225419ee30fdb3620e81122825debb090121020fe041b72b5d713722246f488f4c671f12411b839366c5c8a8589d1432722fdb"},"sequence":4294967295,"n":0,"addr":"1N6o7CGdtufFgReu3QgoRuRKGztqe5iEoJ","valueSat":24836100,"value":0.248361,"doubleSpentTxID":null}],"vout":[{"value":"0.00609050","n":0,"scriptPubKey":{"hex":"76a9140b5e387dc3476aa8b1b6489f7ab2d2b4e6947f1088ac","asm":"OP_DUP OP_HASH160 0b5e387dc3476aa8b1b6489f7ab2d2b4e6947f10 OP_EQUALVERIFY OP_CHECKSIG","addresses":["1237JgGNtNK6xRaF51mvB427KwonmBKMXC"],"type":"pubkeyhash"},"spentTxId":"7f532901f6e2c8271ab1e683e7532964c18eeaeb6dd62827b61cadf8bae0d656","spentIndex":38,"spentHeight":461447},{"value":"0.24199930","n":1,"scriptPubKey":{"hex":"76a914f36d3a194db23e00cfddde82263a0370a56c72d988ac","asm":"OP_DUP OP_HASH160 f36d3a194db23e00cfddde82263a0370a56c72d9 OP_EQUALVERIFY OP_CHECKSIG","addresses":["1PC83qW5GE98TJUhcAqkptbmBRYffkHTXz"],"type":"pubkeyhash"},"spentTxId":"4a91d96f36fc088b65e8e8bd409f29a71348fec4453f7cecaf541daefcf8a541","spentIndex":6,"spentHeight":461544}],"blockhash":"000000000000000000105dab29c262850c957891af4df0f474ed866df86fc12e","blockheight":461446,"confirmations":119,"time":1491926327,"blocktime":1491926327,"valueOut":0.2480898,"size":225,"valueIn":0.248361,"fees":0.0002712}|} ;
  {|{"txid":"cd07745d6632049cf2a38fcf8eee3977a78bd4206aba9b2f86327d947c5ca4cb","version":1,"locktime":0,"vin":[{"txid":"507375cc687172858f960183f90baea8978a08aef5055d84983de9a07cd620ba","vout":1,"scriptSig":{"asm":"3045022100a9734d2e707081d6217c1fc80ee38e9c5a1667a426a2ff8f5c75da30320de25202200fd130373108d47c1ec8717377832906da3616403fe1401e52603703ceadd509[ALL] 026cd44404d016a66498458c7356818755587a553c32cfbdfe74d1d352f888b784","hex":"483045022100a9734d2e707081d6217c1fc80ee38e9c5a1667a426a2ff8f5c75da30320de25202200fd130373108d47c1ec8717377832906da3616403fe1401e52603703ceadd5090121026cd44404d016a66498458c7356818755587a553c32cfbdfe74d1d352f888b784"},"sequence":4294967295,"n":0,"addr":"1CaTdKDD4imFXYJ7Q9Y8aNY32yscHRmW8R","valueSat":2667430,"value":0.0266743,"doubleSpentTxID":null}],"vout":[{"value":"0.02625700","n":0,"scriptPubKey":{"hex":"76a91463940abd17ba3e7ceedfe960d9f19d5f2b9596c788ac","asm":"OP_DUP OP_HASH160 63940abd17ba3e7ceedfe960d9f19d5f2b9596c7 OP_EQUALVERIFY OP_CHECKSIG","addresses":["1A5XF8K7ehBWHZWexddcBJvmdSegZFtGRi"],"type":"pubkeyhash"},"spentTxId":null,"spentIndex":null,"spentHeight":null},{"value":"0.00005570","n":1,"scriptPubKey":{"hex":"76a914ed1fddfca0d4cf287fe0659cad4f3a7af9ce884e88ac","asm":"OP_DUP OP_HASH160 ed1fddfca0d4cf287fe0659cad4f3a7af9ce884e OP_EQUALVERIFY OP_CHECKSIG","addresses":["1NcoKdcVMSN5563GrZNToNJ6hQLUKM1PwT"],"type":"pubkeyhash"},"spentTxId":null,"spentIndex":null,"spentHeight":null}],"blockheight":-1,"confirmations":0,"time":1491989847,"valueOut":0.0263127,"size":226,"valueIn":0.0266743,"fees":0.0003616}|}
]

let print_unknown ppf _ = Format.fprintf ppf "unknown error"

let main () =
  BA.utxos ~testnet:true ["mtaGjUwusMAyx451M26KAJq6a8BiEJyMUd"] >>= function
  | Ok res ->
      List.iter txs ~f:begin fun t ->
        let json = Ezjsonm.from_string t in
        try
          ignore @@ Json_encoding.destruct Tx.encoding json
        with exn -> begin
            error "%s"
              (Format.asprintf "%a"
                 (Json_encoding.print_error ~print_unknown) exn) ;
          end
      end ;
      List.iter res ~f:begin fun utxo ->
        info "%s" (Utxo.to_string utxo)
      end ;
      Scheduler.yield_until_no_jobs_remain () >>= fun () ->
      Shutdown.exit 0
  | Error err ->
      error "%s" (BA.Http.string_of_error err) ;
      Scheduler.yield_until_no_jobs_remain () >>= fun () ->
      Shutdown.exit 1

let () =
  set_level `Debug ;
  don't_wait_for @@ main () ;
  never_returns @@ Scheduler.go ()
