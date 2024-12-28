open Lexing
open Parser
open Ast
open Lexer

let parse_file filename =
  let in_channel = open_in filename in
  let lexbuf = Lexing.from_channel in_channel in
  try
    let ast = Parser.file Lexer.token lexbuf in
    close_in in_channel;
    ast
  with
  | Lexer.LexerError msg ->
      close_in in_channel;
      Printf.eprintf "Lexer error: %s\n" msg;
      exit 1
  | Ast.SyntaxError msg ->
      close_in in_channel;
      Printf.eprintf "Parser error: %s at line %d, column %d\n"
        msg
        lexbuf.lex_curr_p.pos_lnum
        (lexbuf.lex_curr_p.pos_cnum - lexbuf.lex_curr_p.pos_bol + 1);
      exit 1

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    (* Afficher l'AST *)
    Printf.printf "%s\n" (Ast.string_of_file ast)