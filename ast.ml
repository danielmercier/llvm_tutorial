(* expr - Base type for all expression nodes. *)
type expr =
  (* numeric literal integers *)
  | Number of int

  (* variables *)
  | Variable of string

  (* binary operations *)
  | Binary of Token.operator * expr * expr

  (* call to function *)
  | Call of string * expr list

(* proto - prototype of a function, the function name and arguments names *)
type proto = Prototype of string * string list

(* func - the function itself *)
type func = Function of string * expr
