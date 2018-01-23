(* token - Lexems *)
type token =
  (* commands *)
  | Def | Extern

  (* identifiers are strings *)
  | Ident of string

  (* integer numbers *)
  | Number of int

  (* unknown characters *)
  | Kwd of char
