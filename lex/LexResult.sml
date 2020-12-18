(** Copyright (c) 2020 Sam Westrick
  *
  * See the file LICENSE for details.
  *)

structure LexResult =
struct

  datatype error =
    Other of string
  | Error of {pos: Source.t, what: string, explain: string option}

  type t = error

  fun report err =
    case err of
      Other msg =>
        "Uncategorized error: " ^ msg
    | Error {pos, what, explain} =>
        ShowLineError.show {pos=pos, what=what, explain=explain}

  datatype result =
    Success of Token.t Seq.t
  | Failure of {partial: Token.t Seq.t, error: error}

  type t = result

end
