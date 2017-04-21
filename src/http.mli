type error =
  | Cohttp of exn
  | Client of string
  | Server of string
  | API of string
  | Data_encoding of string

type nonrec 'a result = ('a, error) result

val string_of_error : error -> string
val pp_error : Format.formatter -> error -> unit
