let rec lex = parser
  (* whitespaces *)
  | [< ' (' ' | '\n' | 'r' | 't'); stream >] -> lex stream

  (* identifiers: [a-zA-Z][a-zA-Z0-9]* *)
  | [< ' ('A' .. 'Z' | 'a' .. 'z' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_ident buffer stream

  (* numbers: [0-9]+ *)
  | [< ' ('0' .. '9' as c); stream >] ->
      let buffer = Buffer.create 1 in
      Buffer.add_char buffer c;
      lex_number buffer stream

  (* comments: begin with # until end of line*)
  | [< ' ('#'); stream >] ->
      lex_comment stream

  | [< op=lex_operator; stream >] -> [< 'op; lex stream >]

  (* unknown character *)
  | [< 'c; stream >] -> [< 'Token.Unknown c; lex stream >]

  (* end of stream*)
  | [< >] -> [< >]

and lex_ident buffer = parser
  | [< ' ('A' .. 'Z' | 'a' ..  'z' | '0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_ident buffer stream
  | [< stream >] ->
      match Buffer.contents buffer with
      | "def" -> [< 'Token.Def; lex stream >]
      | "extern" -> [< 'Token.Extern; lex stream >]
      | id -> [< 'Token.Ident id; lex stream >]

and lex_number buffer = parser
  | [< ' ('0' .. '9' as c); stream >] ->
      Buffer.add_char buffer c;
      lex_number buffer stream
  | [< stream >] ->
      [< 'Token.Number (int_of_string (Buffer.contents buffer)); lex stream >]

and lex_operator = parser
  | [< ''+' >] -> Token.Operator Token.Add
  | [< ''-' >] -> Token.Operator Token.Minus
  | [< ''*' >] -> Token.Operator Token.Mult
  | [< ''/' >] -> Token.Operator Token.Div

  | [< ''<'; stream >] ->
      let aux = parser
        | [< ''=' >] -> Token.Operator Token.Le
        | [< >] -> Token.Operator Token.Lt
      in
      aux stream

  | [< ''>'; stream >] ->
      let aux = parser
        | [< ''=' >] -> Token.Operator Token.Ge
        | [< >] -> Token.Operator Token.Gt
      in
      aux stream

  | [< ''=' >] -> Token.Operator Token.Eq
  | [< ''!'; ''=' >] -> Token.Operator Token.Neq

  | [< ''a'; ''n'; ''d' >] -> Token.Operator Token.And
  | [< ''o'; ''r' >] -> Token.Operator Token.Or

  (* No operator has been recognized *)
  | [< >] -> raise Stream.Failure;


and lex_comment = parser
   | [< ' ('\n'); stream >] -> lex stream
   | [< '_; stream >] -> lex_comment stream
   | [< >] -> [< >]

(*==========================================================================*)
(*================================TEST======================================*)
(*==========================================================================*)

let rec print_token_stream = parser
  | [< 'token; stream >] ->
      print_string (Token.string_of_token token);
      print_char ' ';
      print_token_stream stream

  | [< >] -> print_endline ""

let main () =
   let stream = lex (Stream.of_channel stdin) in
   print_token_stream stream
