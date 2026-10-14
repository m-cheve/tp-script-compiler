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
    |CteT of cte (* for example 42 is a type whose only element is 42 *)
    |PrimitiveType of primType (* number, boolean or string *)
    |ArrayT of type_ (* t[] (t = type)*)
    |Any
    |ObjectT of (id * type_ option) list (* {i1: t1, ..., in: tn} (t optional) (separator: ',' or ';'), tn optional *)
    |Union of type_ list (* t1 | ... | tn (any value having one of these types is also typeable by the union) *)

type instruction =
    |Empty (* ; *)
    |Expression of expression (* e; *)
    |Block of body list (* {b1 ... bn} (bi = instruction or declaration) *)
    |VarDeclaration of binding list (* var bs (bs = i1 t01 e01, i2 t02 e02, ..., in t0n e0n) *)
    |If of expression * instruction * instruction option (* if (e) i1 else i2 (the else i2 part is optional; if present, it associates with the closest if) *)
    |While of expression * instruction (* while (e) i *)
    |Return of expression option (* return; or return e; *)
and binding =
    |Binding of id * type_ option * expression option (* i t0 e0 (t0 = ': type_' or 'empty') (e0 = '= expression' or 'empty') *)
and declaration =
    |Alias of id * type_ (* type i = t *)
    |LocalLet of binding list (* let bs; *)
    |LocalConst of binding list (* const bs; *)
    |Function of id * binding list * type_ option * body list (* i ( as ) t0 { dis } (if t0 is not empty = ': t') *)
and body =
    |Declaration of declaration
    |Instr of instruction

type file = body list

(* Function to convert a constant to JavaScript *)
let rec cte_to_js c =
  match c with
  | Decimal s | Binary s | Octal s | Hexa s | FloatDecimal s -> s
  | Boolean b -> string_of_bool b
  | String s -> "\"" ^ s ^ "\""

(* Function to convert an expression to JavaScript *)
and expression_to_js expr =
  match expr with
  | Par e -> "(" ^ expression_to_js e ^ ")"
  | Cte c -> cte_to_js c
  | Left (IdLM id) -> id
  | Left (Table (e1, e2)) ->
      expression_to_js e1 ^ "[" ^ expression_to_js e2 ^ "]"
  | Left (SubElement (e, id)) ->
      expression_to_js e ^ "." ^ id
  | Object fields ->
      let fields_js =
        List.map (fun (id, e) -> id ^ ": " ^ expression_to_js e) fields
      in
      "{ " ^ String.concat ", " fields_js ^ " }"
  | Array elems ->
      let elems_js = List.map expression_to_js elems in
      "[" ^ String.concat ", " elems_js ^ "]"
  | Function (fn, args) ->
      let args_js = List.map expression_to_js args in
      expression_to_js fn ^ "(" ^ String.concat ", " args_js ^ ")"
  | Unaire (_, e) -> expression_to_js e
  | Bop (e1, op, e2) ->
      let op_js = match op with
        | Add -> "+"
        | Sub -> "-"
        | Mult -> "*"
        | Div -> "/"
        | Pow -> "**"
      in
      expression_to_js e1 ^ " " ^ op_js ^ " " ^ expression_to_js e2
  | Cond (e1, cond, e2) ->
      let cond_js = match cond with
        | Blt -> "<"
        | Ble -> "<="
        | Gt -> ">"
        | Ge -> ">="
        | Eq -> "=="
        | Diff -> "!="
        | StrictEq -> "==="
        | StrictDiff -> "!=="
      in
      expression_to_js e1 ^ " " ^ cond_js ^ " " ^ expression_to_js e2
  | And (e1, e2) -> expression_to_js e1 ^ " && " ^ expression_to_js e2
  | Or (e1, e2) -> expression_to_js e1 ^ " || " ^ expression_to_js e2
  | Affectation (IdLM id, e) -> id ^ " = " ^ expression_to_js e
  | _ -> ""

(* Function to convert an instruction to JavaScript *)
and instruction_to_js instr =
  match instr with
  | Empty -> ";"
  | Expression e -> expression_to_js e ^ ";"
  | Block body ->
      let body_js = List.map body_to_js body in
      "{\n" ^ String.concat "\n" body_js ^ "\n}"
  | VarDeclaration bindings ->
      let bindings_js =
        List.map
          (fun (Binding (id, _, e_opt)) ->
             id ^ (match e_opt with Some e -> " = " ^ expression_to_js e | None -> ""))
          bindings
      in
      "var " ^ String.concat ", " bindings_js ^ ";"
  | If (cond, i1, i2_opt) ->
      "if (" ^ expression_to_js cond ^ ") " ^ instruction_to_js i1 ^
      (match i2_opt with Some i2 -> " else " ^ instruction_to_js i2 | None -> "")
  | While (cond, i) ->
      "while (" ^ expression_to_js cond ^ ") " ^ instruction_to_js i
  | Return e_opt ->
      "return" ^
      (match e_opt with Some e -> " " ^ expression_to_js e | None -> "") ^ ";"

(* Function to convert a declaration to JavaScript *)
and declaration_to_js decl =
  match decl with
  | Alias _ -> ""  (* Ignore aliases *)
  | LocalLet bindings ->
      let bindings_js =
        List.map
          (fun (Binding (id, _, e_opt)) ->
             "let " ^ id ^
             (match e_opt with Some e -> " = " ^ expression_to_js e | None -> ""))
          bindings
      in
      String.concat ";\n" bindings_js ^ ";"
  | LocalConst bindings ->
      let bindings_js =
        List.map
          (fun (Binding (id, _, e_opt)) ->
             "const " ^ id ^
             (match e_opt with Some e -> " = " ^ expression_to_js e | None -> ""))
          bindings
      in
      String.concat ";\n" bindings_js ^ ";"
  | Function (id, bindings, _, body) ->
      let args_js =
        String.concat ", " (List.map (fun (Binding (id, _, _)) -> id) bindings)
      in
      let body_js = List.map body_to_js body in
      "function " ^ id ^ "(" ^ args_js ^ ") {\n" ^
      String.concat "\n" body_js ^ "\n}"

(* Function to convert a body to JavaScript *)
and body_to_js body =
  match body with
  | Declaration decl -> declaration_to_js decl
  | Instr instr -> instruction_to_js instr

(* Main function to output a file in JavaScript *)
let ast_to_js file =
  let js_code = List.map body_to_js file in
  String.concat "\n" js_code