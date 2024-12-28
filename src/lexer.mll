{
  open Parser
  open Ast
  exception LexerError of string
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }  (* Ignorer les espaces blancs *)
  | "//" [^ '\n']* '\n' { token lexbuf }  (* Commentaires ligne *)
  | "/*" { comment lexbuf }  (* Commentaires bloc *)
  | ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']* as id {
      match id with
      | "var" -> Var
      | "let" -> Let
      | "const" -> Const
      | "type" -> Type
      | "if" -> If
      | "else" -> Else
      | "while" -> While
      | "return" -> Return
      | "typeof" -> Typeof
      | "true" -> Boolean true
      | "false" -> Boolean false
      | "number" -> Number
      | "boolean" -> BooleanType
      | "string" -> StringType
      | "any" -> Any
      | _ -> Id id
    }
  | ['0'-'9']+ ['_' '0'-'9']* as num {
      Decimal num
    }
  | "0b" ['0'-'1']+ ['_' '0'-'1']* as num { Binary num }
  | "0o" ['0'-'7']+ ['_' '0'-'7']* as num { Octal num }
  | "0x" ['0'-'9' 'a'-'f' 'A'-'F']+ ['_' '0'-'9' 'a'-'f' 'A'-'F']* as num {
      Hexa num
    }
  | ['0'-'9']+ "." ['0'-'9']* ['e' 'E']? ['+' '-']? ['0'-'9']+ ['_' '0'-'9']* as num {
      FloatDecimal num
    }
  | "\"" ([^ '"' '\\'] | '\\' '.')* "\"" as s {
      String (String.sub s 1 (String.length s - 2))
    }
  | "'" ([^ '\'' '\\'] | '\\' '.')* "'" as s {
      String (String.sub s 1 (String.length s - 2))
    }
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
  | "*/" { token lexbuf }  (* Fin du commentaire *)
  | _ { comment lexbuf }   (* Continuer à consommer *)
  | eof { raise (LexerError "Unclosed comment") }

{
  let from_channel = Lexing.from_channel
}
