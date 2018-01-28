(* binop_precedence - This holds the precedence for each binary operator that
 * is defined *)
let binop_precedence: (Token.operator, int) Hashtbl.t = Hashtbl.create 10

(* by default, associativity is left *)
let right_associative: (Token.operator, bool) Hashtbl.t = Hashtbl.create 10


(* precedence - Get the precedence of the pending binary operator token.
 *              c should be known (is_known c) *)
let precedence c = Hashtbl.find binop_precedence c

let is_left_associative c = not (Hashtbl.mem right_associative c
                                 && Hashtbl.find right_associative c)

(* function prototypes *)
let function_table: (string, Ast.proto) Hashtbl.t = Hashtbl.create 10

(* is_known - Return true if the operator is known *)
let is_known c = Hashtbl.mem binop_precedence c

let rec parse_expr = parser
  | [< p=parse_primary; stream >] -> parse_bin_rhs 0 p stream
  | [< >] -> raise Stream.Failure

(* primary
 *    ::= identifier
 *    ::= numberexpr
 *    ::= parenexpr *)
and parse_primary = parser
  (* identifier *)
  | [< 'Token.Ident id; stream >] -> parse_ident id stream

  (* numberexpr *)
  | [< 'Token.Number n >] -> Ast.Number n

  (* parenexpr *)
  | [< 'Token.Unknown '('; e=parse_expr; 'Token.Unknown ')' ?? "expected ')'" >] -> e

  (* expecting a primary expression *)
  | [< >] -> raise Stream.Failure

and parse_bin_rhs prec lhs stream =
  match Stream.peek stream with
  | Some (Token.Operator op) when is_known op ->
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
          | Some (Token.Operator op2) when is_known op2 ->
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

and parse_ident id = parser
  | [< 'Token.Unknown '('; arg_list=parse_args;
       'Token.Unknown ')' ?? "expected ')'" >] ->

      (* let's match a function call *)
      let name_array =
        match Hashtbl.find function_table id with
        | Ast.Prototype (_, name_list) -> name_list
      in
      let expr_array = Array.of_list arg_list in
      if Array.length name_array != Array.length expr_array then
        raise (Stream.Error "Not the same number of arguments expected");
      Ast.Call (id, expr_array)

  | [< >] -> Ast.Variable id

and parse_args = parser
  | [< expr=parse_expr; stream >] ->
      begin
        match Stream.peek stream with
        | Some (Token.Unknown ',') ->
            Stream.junk stream;
            expr :: (parse_args stream)
        | _ -> [expr];
      end

  | [<  >] -> raise (Stream.Error "Expecting an argument")

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

        (* variables *)
        | Ast.Call (id, args) ->
            print_string id;
            print_char '(';

            Array.iteri
              (fun i e ->
                aux e;
                if i + 1 < Array.length args then
                  print_string ", ")
              args;

            print_char ')'

        | Ast.Binary(op, lhs, rhs)  ->
            print_char '(';
            aux lhs;
            print_char ' ';
            print_string (Token.string_of_operator op);
            print_char ' ';
            aux rhs;
            print_char ')'
      in
      aux e;
      print_endline "";
      print_ast stream

  | [< 'Token.Unknown ';'; stream >] ->
      print_ast stream

  | [< >] -> print_endline ""

  (*(* binary operations *)
  | Binary of char * expr * expr

  (* call to function *)
  | Call of string * expr list*)

let main () =
  Hashtbl.add binop_precedence Token.Lt 10;
  Hashtbl.add binop_precedence Token.Add 20;
  Hashtbl.add binop_precedence Token.Minus 20;
  Hashtbl.add binop_precedence Token.Mult 40;

  Hashtbl.add function_table "foo" (Ast.Prototype ("foo", [|"x"|]));

  let stream = Lexer.lex (Stream.of_channel stdin) in
  print_ast stream;
  print_endline ""
