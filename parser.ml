(* binop_precedence - This holds the precedence for each binary operator that
 * is defined *)
let binop_precedence: (char, int) Hashtbl.t = Hashtbl.create 10

(* by default, associativity is left *)
let right_associative: (char, bool) Hashtbl.t = Hashtbl.create 10


(* precedence - Get the precedence of the pending binary operator token.
 *              c should be known (is_known c) *)
let precedence c = Hashtbl.find binop_precedence c

let is_left_associative c = not (Hashtbl.mem right_associative c
                                 && Hashtbl.find right_associative c)

(* is_known - Return true if the operator is known *)
let is_known c = Hashtbl.mem binop_precedence c

let rec parse_expr = parser
  | [< p=parse_primary; stream >] -> parse_bin_rhs 0 p stream
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

and parse_bin_rhs prec lhs stream =
  match Stream.peek stream with
  | Some (Token.Kwd op) when is_known op ->
      let op_prec = precedence op in

      if op_prec < prec || (op_prec = prec && is_left_associative op)
      then lhs
      else begin
        (* eat the operator *)
        Stream.junk stream;

        (* get the next primary expression *)
        let rhs = parse_primary stream in

        let rhs =
          (* lookahead for the next operator and its precedence *)
          match Stream.peek stream with
          | Some (Token.Kwd op2) when is_known op2 ->
              let op2_prec = precedence op2 in
              (* if the precedence of this operator is less than the previous,
               * the rhs is just the primary expression, else we recurse to
               * get the rhs *)
              if (op2_prec < op_prec
                  || (op2_prec = op_prec && is_left_associative op))
              then rhs
              else parse_bin_rhs op_prec rhs stream
          | _ -> rhs
        in

        let bin = Ast.Binary (op, lhs, rhs) in
        parse_bin_rhs prec bin stream
      end
  | _ -> lhs

(*==========================================================================*)
(*================================TEST======================================*)
(*==========================================================================*)

let rec print_ast = parser
  | [< e=parse_expr; stream >] ->
      let rec aux = function
        | Ast.Number n ->
            print_int n;

        (* variables *)
        | Ast.Variable id ->
            print_string id

        | Ast.Binary(op, lhs, rhs)  ->
            print_char '(';
            aux lhs;
            print_char ' ';
            print_char op;
            print_char ' ';
            aux rhs;
            print_char ')'

        | _ -> assert false
      in
      aux e;
      print_endline "";
      print_ast stream

  | [< >] -> print_endline ""

  (*(* binary operations *)
  | Binary of char * expr * expr

  (* call to function *)
  | Call of string * expr list*)

let main () =
  Hashtbl.add binop_precedence '<' 10;
  Hashtbl.add binop_precedence '+' 20;
  Hashtbl.add binop_precedence '-' 20;
  Hashtbl.add binop_precedence '*' 40;

  let stream = Lexer.lex (Stream.of_channel stdin) in
  print_ast stream;
  print_endline ""
