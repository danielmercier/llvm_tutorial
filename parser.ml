(* binop_precedence - This holds the precedence for each binary operator that
 * is defined *)
let binop_precedence: (char, int) Hashtbl.t = Hashtbl.create 10

(* precedence - Get the precedence of the pending binary operator token. *)
let precedence c = if Hashtbl.mem binop_precedence c then
                     Some (Hashtbl.find binop_precedence c)
                   else
                     None

let rec parse_expr = parser
  | [< p=parse_primary; stream >] -> p
  | [< >] -> raise (Stream.Error "not expecting end of stream")

(* primary 
 *    ::= identifier
 *    ::= numberexpr
 *    ::= parenexpr *)
and parse_primary = parser
  (* identifier *)
  | [< 'Token.Ident id >] -> Ast.Variable id

  (* numberexpr *)
  | [< 'Token.Number n >] -> Ast.Number n

  (* parenexpr *)
  | [< 'Token.Kwd '('; e=parse_expr; 'Token.Kwd ')' ?? "expected ')'" >] -> e

  (* expecting a primary expression *)
  | [< >] -> raise (Stream.Error "unknown token when expecting an expression.")

and parse_lhs = parser
  | [< prim=parse_primary; stream >] ->
      

(* try to parse the rhs of a binop expression *)
and parse_rhs lhs_precedence lhs_expr = parser
  | [< 'Token.Kwd op; prim=parse_primary; stream >] ->
      match precedence op with
      | Some op_precedence ->
          (* Greater or equal means left associativity *)
          if lhs_precedence >= op_precedence then
            parse_rhs lhs_expr
          else
            parse_rhs op_precedence prim

      
      (* It's a no match if the operator is not recognized *)
      | None -> raise Stream.Failure;



(*==========================================================================*)
(*================================TEST======================================*)
(*==========================================================================*)

let rec print_ast = parser
  | [< e=parse_expr; stream >] ->
      begin
        match e with
        | Ast.Number n ->
            print_int n;
            print_char ' '

        (* variables *)
        | Ast.Variable id ->
            print_string (id ^ " ")

        | _ -> assert false
      end;
      print_endline "";
      print_ast stream

  | [< >] -> print_endline ""

  (*(* binary operations *)
  | Binary of char * expr * expr

  (* call to function *)
  | Call of string * expr list*)

let main () =
  Hashtbl.add binop_precedence '/' 10;
  Hashtbl.add binop_precedence '+' 20;
  Hashtbl.add binop_precedence '-' 20;
  Hashtbl.add binop_precedence '*' 40;

  let stream = Lexer.lex (Stream.of_channel stdin) in
  print_ast stream;
  print_endline ""
