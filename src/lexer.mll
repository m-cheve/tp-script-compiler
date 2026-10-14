{
  open Parser
  open Ast
  exception LexerError of string
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }  (* Ignore whitespace *)
  | "//"  [^ '\n']* eof { token lexbuf }  (* Line comments at end of file *)
  | "//" [^ '\n']* '\n' { token lexbuf }  (* Line comments *)
  | "/*" { comment lexbuf }  (* Block comments *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
      match id with
      | "function" -> FunctionToken
      | "var" -> Var
      | "let" -> Let
      | "const" -> Const
      | "type" -> Type
      | "if" -> If
      | "else" -> Else
      | "while" -> While
      | "return" -> Return
      | "typeof" -> UTypeof
      | "true" -> Boolean true
      | "false" -> Boolean false
      | "number" -> Number
      | "boolean" -> BooleanType
      | "string" -> StringType
      | "any" -> Any
      | _ -> Id id
    }
  | '0' | (['1'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*) as num {
      Decimal num
    }
  | '0' ['b' 'B'] ['0'-'1']+ (['_'] ['0'-'1'] | ['0'-'1'])* as num { Binary num }
  | '0' ['o' 'O'] ['0'-'7']+ (['_'] ['0'-'7'] | ['0'-'7'])* as num { Octal num }
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+ (['_'] ['0'-'9' 'a'-'f' 'A'-'F'] | ['0'-'9' 'a'-'f' 'A'-'F'])* as num {
      Hexa num
    }
  | ('0' | (['1'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*)) '.' (['0'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*)? (['e' 'E'] ['+' '-']? ['0'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*)? as num {
      FloatDecimal num
    }
  | '.' ['0'-'9']+ ('_'? ['0'-'9']+)* (['e' 'E'] ['+' '-']? ['0'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*)? as num {
      FloatDecimal num
    }
  | ('0' | (['1'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])*)) ['e' 'E'] ['+' '-']? ['0'-'9']+ (['_'] ['0'-'9'] | ['0'-'9'])* as num {
      FloatDecimal num
    }
  | "\"" { String (double_quote_string "" lexbuf) }  (* Start of a string literal, starting with an empty string *)
  | "'" { String (single_quote_string "" lexbuf) }  (* Start of a string literal, starting with an empty string *)
  | "==" { Eq }
  | "!=" { Diff }
  | "===" { StrictEq }
  | "!==" { StrictDiff }
  | "<" { Blt }
  | "<=" { Ble }
  | ">" { Gt }
  | ">=" { Ge }
  | "+" { Plus }
  | "-" { Minus }
  | "*" { Mult }
  | "/" { Div }
  | "**" { Pow }
  | "&&" { And }
  | "||" { Or }
  | "=" { Equals }
  | ":" { Colon }
  | ";" { Semicolon }
  | "," { Comma }
  | "." { Dot }
  | "(" { LPar }
  | ")" { RPar }
  | "{" { LBrace }
  | "}" { RBrace }
  | "[" { LBracket }
  | "]" { RBracket }
  | "|" { Pipe }
  | eof { EOF }
  | _ as c { raise (LexerError (Printf.sprintf "Unexpected character: %c" c)) }

and comment = parse
  | "*/" { token lexbuf }  (* End of comment *)
  | _ { comment lexbuf }   (* Continue consuming *)
  | eof { raise (LexerError "Unclosed comment") }

and double_quote_string acc = parse
  | "\"" { acc }  (* End of string literal, return accumulator *)
  | "\\\"" as c { double_quote_string (acc ^ c) lexbuf }  (* Add escaped quote and continue accumulating *)
  | _ as c { double_quote_string (acc ^ (String.make 1 c)) lexbuf }  (* Accumulate characters other than quotes *)
  | eof { raise (LexerError "Unclosed string") }  (* If the string is not closed before end of file *)

and single_quote_string acc = parse
  | "'" { acc }  (* End of string, return accumulator *)
  | "\\'" as c { single_quote_string (acc ^ c) lexbuf }  (* Add apostrophe and continue accumulating *)
  | _ as c { single_quote_string (acc ^ (String.make 1 c)) lexbuf }  (* Accumulate valid characters *)
  | eof { raise (LexerError "Unclosed single-quoted string") }  (* If the string is not closed before end of file *)

{
  let from_channel = Lexing.from_channel
}
