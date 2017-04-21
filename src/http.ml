type error =
  | Cohttp of exn
  | Client of string
  | Server of string
  | API of string
  | Data_encoding of string

let api_str msg = API msg
let api k =
  Format.kasprintf (fun msg -> API msg) k

let api_fail msg = Error (API msg)

let api_failf k =
  Format.kasprintf (fun msg -> Error (API msg)) k

let data_encoding json = Error (Data_encoding json)

let string_of_error = function
  | Cohttp exn -> Printexc.to_string exn
  | Client msg -> "HTTP Client error: " ^ msg
  | Server msg -> "HTTP Server error: " ^ msg
  | API msg -> "API error: " ^ msg
  | Data_encoding msg -> "Data encoding error: " ^ msg

let pp_error ppf t =
  Format.fprintf ppf "%s" (string_of_error t)

type nonrec 'a result = ('a, error) result
