(* operator - All operators *)
type operator =
  (* + *)
  | Add
  (* - *)
  | Minus
  (* * *)
  | Mult
  (* / *)
  | Div

  (* < *)
  | Lt
  (* <= *)
  | Le
  (* > *)
  | Gt
  (* >= *)
  | Ge

  (* = *)
  | Eq
  (* != *)
  | Neq

  (* and *)
  | And
  (* or *)
  | Or

(* token - Lexems *)
type token =
  (* commands *)
  | Def | Extern

  (* identifiers are strings *)
  | Ident of string

  (* integer numbers *)
  | Number of int

  (* operator *)
  | Operator of operator

  (* unknown characters *)
  | Unknown of char

let string_of_operator = function
  | Add -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Lt -> "<"
  | Le -> "<="
  | Gt -> ">"
  | Ge -> ">="
  | Eq -> "="
  | Neq -> "!="
  | And -> "and"
  | Or -> "or"

let string_of_token = function
  | Def -> "def"
  | Extern -> "extern"
  | Ident id -> id
  | Number n -> string_of_int n
  | Operator op -> string_of_operator op
  | Unknown c -> String.make 1 c
