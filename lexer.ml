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

  (* unknown character *)
  | [< 'c; stream >] -> [< 'Token.Kwd c; lex stream >]

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

and lex_comment = parser
   | [< ' ('\n'); stream >] -> lex stream
   | [< '_; stream >] -> lex_comment stream
   | [< >] -> [< >]


(*==========================================================================*)
(*================================TEST======================================*)
(*==========================================================================*)

let rec print_token_stream = parser
  | [< 'Token.Def; stream >] ->
      print_string "def ";
      print_token_stream stream

  | [< 'Token.Extern; stream >] ->
      print_string "extern ";
      print_token_stream stream

  | [< 'Token.Ident id; stream >] ->
      print_string id;
      print_char ' ';
      print_token_stream stream

  | [< 'Token.Number n; stream >] ->
      print_int n;
      print_char ' ';
      print_token_stream stream

  | [< 'Token.Kwd c; stream >] ->
      print_char c;
      print_char ' ';
      if c = ';' then
        print_endline "";
      print_token_stream stream

  | [< >] -> print_endline ""

let main () =
   let stream = lex (Stream.of_channel stdin) in
   print_token_stream stream
