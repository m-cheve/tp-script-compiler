exception SyntaxError of string

type comment =
    |Line of string (* //... *)
    |Group of string (* /* ... */ *)

type cte =
    |Decimal of string
    |Binary of string
    |Octal of string
    |Hexa of string
    |FloatDecimal of string
    |Boolean of bool
    |String of string

type id = string

type unaire =
    |UTypeof |Plus |Minus

type bop =
    |Pow |Mult |Div
    |Add |Sub

type cond =
    |Blt |Ble |Gt |Ge (* <, <=, >, >= *)
    |Eq |Diff (* ==, != *)
    |StrictEq |StrictDiff (* ===, !== *)

type expression =
    |Par of expression (* (e) *)
    |Cte of cte
    |Left of left_Member
    |Object of (id * expression) list (* {i1: e1, ..., in: en} *)
    |Array of expression list (* [e1, ..., en] *)
    |Function of expression * expression list (* e(e1, ..., en) *)
    |Unaire of unaire * expression
    |Bop of expression * bop * expression
    |Cond of expression * cond * expression
    |And of expression * expression
    |Or of expression * expression
    |Affectation of left_Member * expression (* l = e *)
and left_Member =
    |IdLM of id
    |Table of expression * expression (* e[e] *)
    |SubElement of expression * id (* e.i *)

type primType=
    |Number
    |Boolean
    |String

type type_ =
    |IdT of id
    |CteT of cte (* par exemple 42 est un type dont le seul élément est 42 *)
    |PrimitiveType of primType (* number, boolean ou string *)
    |ArrayT of type_ (* t[] (t = type)*)
    |Any
    |ObjectT of (id * type_ option) list (* {i1: t1, ..., in: tn} (t en option) (séparation : ',' ou ';'), tn en option*)
    |Union of type_ list (* t1 | ... | tn  ( toute valeur qui possède un de ces types est aussi typable par l’union)*)

type instruction =
    |Empty (* ; *)
    |Expression of expression (* e; *)
    |Block of body list (* {b1 ... bn} (bi = instruction ou déclaration)*)
    |VarDeclaration of binding list (* var bs (bs = i1 t01 e01, i2 t02 e02, ..., in t0n e0n)*)
    |If of expression * instruction * instruction option (* if (e) i1 else i2 (la partie else i2 est optionnelle ; si elle est présente, elle se rapporte au if le plus proche)*)
    |While of expression * instruction (* while (e) i *)
    |Return of expression option (* return; ou return e; *)
and binding =
    |Binding of id * type_ option * expression option (* i t0 e0 (t0 = ': type_' ou 'vide') (e0 = '= expression' ou 'vide')*)
and declaration =
    |Alias of id * type_ (* type i = t *)
    |LocalLet of binding list (* let bs; *)
    |LocalConst of binding list (* const bs; *)
    |Function of id * binding list * type_ option * body list (* i ( as ) t0 { dis } (si t0 n'est pas vide = ': t')*)
and body =
    |Declaration of declaration
    |Instr of instruction

type file = body list

let rec string_of_cte = function
  | Decimal s -> "Decimal(" ^ s ^ ")"
  | Binary s -> "Binary(" ^ s ^ ")"
  | Octal s -> "Octal(" ^ s ^ ")"
  | Hexa s -> "Hexa(" ^ s ^ ")"
  | FloatDecimal s -> "FloatDecimal(" ^ s ^ ")"
  | Boolean b -> "Boolean(" ^ string_of_bool b ^ ")"
  | String s -> "String(" ^ s ^ ")"

let rec string_of_expression = function
  | Par e -> "Par(" ^ string_of_expression e ^ ")"
  | Cte c -> "Cte(" ^ string_of_cte c ^ ")"
  | Left lm -> "Left(" ^ string_of_left_member lm ^ ")"
  | Object lst -> "Object(" ^ String.concat ", " (List.map (fun (id, e) -> id ^ ": " ^ string_of_expression e) lst) ^ ")"
  | Array lst -> "Array(" ^ String.concat ", " (List.map string_of_expression lst) ^ ")"
  | Function (e, lst) -> "Function(" ^ string_of_expression e ^ ", [" ^ String.concat ", " (List.map string_of_expression lst) ^ "])"
  | Unaire (u, e) -> "Unaire(" ^ string_of_unaire u ^ ", " ^ string_of_expression e ^ ")"
  | Bop (e1, b, e2) -> "Bop(" ^ string_of_expression e1 ^ ", " ^ string_of_bop b ^ ", " ^ string_of_expression e2 ^ ")"
  | Cond (e1, c, e2) -> "Cond(" ^ string_of_expression e1 ^ ", " ^ string_of_cond c ^ ", " ^ string_of_expression e2 ^ ")"
  | And (e1, e2) -> "And(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
  | Or (e1, e2) -> "Or(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
  | Affectation (lm, e) -> "Affectation(" ^ string_of_left_member lm ^ ", " ^ string_of_expression e ^ ")"

and string_of_left_member = function
  | IdLM id -> "IdLM(" ^ id ^ ")"
  | Table (e1, e2) -> "Table(" ^ string_of_expression e1 ^ ", " ^ string_of_expression e2 ^ ")"
  | SubElement (e, id) -> "SubElement(" ^ string_of_expression e ^ ", " ^ id ^ ")"

and string_of_unaire = function
  | UTypeof -> "UTypeof"
  | Plus -> "Plus"
  | Minus -> "Minus"

and string_of_bop = function
  | Pow -> "Pow"
  | Mult -> "Mult"
  | Div -> "Div"
  | Add -> "Add"
  | Sub -> "Sub"

and string_of_cond = function
  | Blt -> "Blt"
  | Ble -> "Ble"
  | Gt -> "Gt"
  | Ge -> "Ge"
  | Eq -> "Eq"
  | Diff -> "Diff"
  | StrictEq -> "StrictEq"
  | StrictDiff -> "StrictDiff"

let rec string_of_file file =
  String.concat "\n" (List.map string_of_body file)

and string_of_body = function
  | Declaration d -> "Declaration(" ^ string_of_declaration d ^ ")"
  | Instr i -> "Instr(" ^ string_of_instruction i ^ ")"

and string_of_declaration = function
  | Alias (id, t) -> "Alias(" ^ id ^ ", " ^ string_of_type t ^ ")"
  | LocalLet lst -> "LocalLet(" ^ String.concat ", " (List.map string_of_binding lst) ^ ")"
  | LocalConst lst -> "LocalConst(" ^ String.concat ", " (List.map string_of_binding lst) ^ ")"
  | Function (id, lst, t_opt, body_lst) ->
      "Function(" ^ id ^ ", [" ^ String.concat ", " (List.map string_of_binding lst) ^ "], " ^
      (match t_opt with Some t -> string_of_type t | None -> "None") ^ ", [" ^
      String.concat ", " (List.map string_of_body body_lst) ^ "])"

and string_of_binding = function
  | Binding (id, t_opt, e_opt) ->
      "Binding(" ^ id ^ ", " ^
      (match t_opt with Some t -> string_of_type t | None -> "None") ^ ", " ^
      (match e_opt with Some e -> string_of_expression e | None -> "None") ^ ")"

and string_of_instruction = function
  | Empty -> "Empty"
  | Expression e -> "Expression(" ^ string_of_expression e ^ ")"
  | Block lst -> "Block([" ^ String.concat ", " (List.map string_of_body lst) ^ "])"
  | VarDeclaration lst -> "VarDeclaration([" ^ String.concat ", " (List.map string_of_binding lst) ^ "])"
  | If (e, i1, i2_opt) ->
      "If(" ^ string_of_expression e ^ ", " ^ string_of_instruction i1 ^ ", " ^
      (match i2_opt with Some i2 -> string_of_instruction i2 | None -> "None") ^ ")"
  | While (e, i) -> "While(" ^ string_of_expression e ^ ", " ^ string_of_instruction i ^ ")"
  | Return e_opt -> "Return(" ^ (match e_opt with Some e -> string_of_expression e | None -> "None") ^ ")"

and string_of_type = function
  | IdT id -> "IdT(" ^ id ^ ")"
  | CteT c -> "Cte(" ^ string_of_cte c ^ ")"
  | PrimitiveType pt -> "PrimitiveType(" ^ string_of_primType pt ^ ")"
  | ArrayT t -> "Array(" ^ string_of_type t ^ ")"
  | Any -> "Any"
  | ObjectT lst -> "Object([" ^ String.concat ", " (List.map (fun (id, t_opt) -> id ^ ": " ^ (match t_opt with Some t -> string_of_type t | None -> "None")) lst) ^ "])"
  | Union lst -> "Union([" ^ String.concat ", " (List.map string_of_type lst) ^ "])"

and string_of_primType = function
  | Number -> "Number"
  | Boolean -> "Boolean"
  | String -> "String"