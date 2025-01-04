{
  open Parser
  open Ast
  exception LexerError of string
}

rule token = parse
  | [' ' '\t' '\n' '\r'] { token lexbuf }  (* Ignorer les espaces blancs *)
  | "//"  [^ '\n']* eof { token lexbuf }  (* Commentaires ligne en fin de fichier *)
  | "//" [^ '\n']* '\n' { token lexbuf }  (* Commentaires ligne *)
  | "/*" { comment lexbuf }  (* Commentaires bloc *)
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
  | ['0'-'9']+ ['_' '0'-'9']* as num {
      Decimal num
    }
  | '0' ['b' 'B'] ['0'-'1']+ ['_' '0'-'1']* as num { Binary num }
  | '0' ['o' 'O'] ['0'-'7']+ ['_' '0'-'7']* as num { Octal num }
  | '0' ['x' 'X'] ['0'-'9' 'a'-'f' 'A'-'F']+ ['_' '0'-'9' 'a'-'f' 'A'-'F']* as num {
      Hexa num
    }
  | ['0'-'9']+ ('_'? ['0'-'9']+)* '.' ['0'-'9']* ('_'? ['0'-'9']+)* (['e' 'E'] ['+' '-']? ['0'-'9']+ ('_'? ['0'-'9']+)*)? as num {
      FloatDecimal num
    }
  | '.' ['0'-'9']+ ('_'? ['0'-'9']+)* (['e' 'E'] ['+' '-']? ['0'-'9']+ ('_'? ['0'-'9']+)*)? as num {
      FloatDecimal num
    }
  | ['0'-'9']+ ('_'? ['0'-'9']+)* ['e' 'E'] ['+' '-']? ['0'-'9']+ ('_'? ['0'-'9']+)* as num {
      FloatDecimal num
    }
  | "\"" { String (double_quote_string "" lexbuf) }  (* Début d'une chaîne de caractères, on commence avec une chaîne vide *)
  | "'" { String (single_quote_string "" lexbuf) }  (* Début d'une chaîne de caractères, on commence avec une chaîne vide *)
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

and double_quote_string acc = parse
  | "\"" { acc }  (* Fin de la chaîne de caractères, retourne l'accumulateur *)
  | "\\\"" as c { double_quote_string (acc ^ c) lexbuf }  (* Ajoute les guillemets et continue à accumuler *)
  | _ as c { double_quote_string (acc ^ (String.make 1 c)) lexbuf }  (* Accumule les caractères autres que les guillemets *)
  | eof { raise (LexerError "Unclosed string") }  (* Si la chaîne n'est pas fermée avant la fin du fichier *)

and single_quote_string acc = parse
  | "'" { acc }  (* Fin de la chaîne, retourne l'accumulateur *)
  | "\\'" as c { single_quote_string (acc ^ c) lexbuf }  (* Ajoute l'apostrophe et continue à accumuler *)
  | _ as c { single_quote_string (acc ^ (String.make 1 c)) lexbuf }  (* Accumule les caractères valides *)
  | eof { raise (LexerError "Unclosed single-quoted string") }  (* Si la chaîne n'est pas fermée avant la fin du fichier *)

{
  let from_channel = Lexing.from_channel
}
