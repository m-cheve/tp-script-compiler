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


(* ----------------------- Question 3, check_scope ----------------------- *)
exception ScopeError of string

(* Définition des types pour suivre les variables *)
type variable_info = {
  mutable is_const: bool;  (* Indique si la variable est déclarée comme const *)
  mutable is_declared: bool;  (* Indique si la variable a été déclarée *)
}

(* Pile des environnements pour gérer la portée *)
type scope = (id, variable_info) Hashtbl.t
let new_scope () = Hashtbl.create 10

(* Fonction principale *)
let check_scope (ast: file) =
  (* Pile des environnements *)
  let scope_stack = Stack.create () in

  (* Entrer dans une nouvelle portée *)
  let enter_scope () = Stack.push (new_scope ()) scope_stack in
  (* Sortir d'une portée *)
  let exit_scope () = ignore (Stack.pop scope_stack) in

  (* Chercher une variable dans les portées *)
  let find_variable id =
    let rec aux stack =
      if Stack.is_empty stack then None
      else
        let current_scope = Stack.top stack in
        match Hashtbl.find_opt current_scope id with
        | Some info -> Some info
        | None ->
          (* Si la variable n'est pas trouvée dans cette portée, continuer *)
          aux (Stack.copy stack |> fun s -> ignore (Stack.pop s); s)
    in
    aux scope_stack
  in

  (* Ajouter une variable à la portée actuelle *)
  let add_variable id is_const =
    match Stack.top_opt scope_stack with
    | Some current_scope ->
      Hashtbl.add current_scope id { is_const; is_declared = true }
    | None -> failwith "Aucune portée active pour déclarer une variable"
  in

  (* Vérification récursive d'un corps *)
  let rec check_body body =
    match body with
    | Declaration decl -> check_declaration decl
    | Instr instr -> check_instruction instr

  (* Vérification d'une déclaration *)
  and check_declaration = function
  | Alias (_, _) -> ()  (* Pas d'impact sur les identifiants *)
  | LocalLet bindings ->
      List.iter (fun (Binding (id, _, _)) -> add_variable id false) bindings
  | LocalConst bindings ->
      List.iter (fun (Binding (id, _, _)) -> add_variable id true) bindings
  | Function (id, bindings, _, body_list) ->
      (* Ajouter la fonction elle-même à la portée avant de traiter ses paramètres et son corps *)
      add_variable id false;  (* La fonction elle-même *)
      (* Entrer dans une nouvelle portée pour la fonction *)
      enter_scope ();
      List.iter (fun (Binding (id, _, _)) -> add_variable id false) bindings;  (* Ajouter les paramètres de la fonction *)
      List.iter check_body body_list;  (* Vérifier le corps de la fonction *)
      exit_scope ()  (* Sortir de la portée de la fonction *)


  (* Vérification d'une instruction *)
  and check_instruction = function
  | Empty -> ()
  | Expression expr -> check_expression expr
  | Block body_list ->
    enter_scope ();
    List.iter check_body body_list;
    exit_scope ()
  | VarDeclaration bindings ->
    (* Ajouter immédiatement les variables à la portée pour var *)
    List.iter (fun (Binding (id, _, expr_opt)) ->
      add_variable id false;  (* Ajout immédiat pour var *)
      Option.iter check_expression expr_opt
    ) bindings
  | If (cond, if_instr, else_instr_opt) ->
    check_expression cond;
    check_instruction if_instr;
    Option.iter check_instruction else_instr_opt
  | While (cond, instr) ->
    check_expression cond;
    check_instruction instr
  | Return expr_opt -> Option.iter check_expression expr_opt


  (* Vérification d'une expression *)
  and check_expression = function
    | Cte _ -> ()
    | Left left_Member -> ( match left_Member with
      | IdLM id ->
        begin match find_variable id with
        | None -> raise (ScopeError ("Variable utilisée sans déclaration : " ^ id))
        | Some { is_declared = false; _ } ->
          raise (ScopeError ("Variable utilisée avant sa déclaration : " ^ id))
        | _ -> ()
        end
      | Table (expr1, expr2) ->
        check_expression expr1;
        check_expression expr2
      | SubElement (expr, _) -> check_expression expr )
    | Function (expr, args) ->
      check_expression expr;
      List.iter check_expression args
    | Unaire (_, expr) -> check_expression expr
    | Bop (expr1, _, expr2) ->
      check_expression expr1;
      check_expression expr2
    | Cond (expr1, _, expr2) ->
      check_expression expr1;
      check_expression expr2
    | And (expr1, expr2)
    | Or (expr1, expr2) ->
      check_expression expr1;
      check_expression expr2
    | Affectation (IdLM id, expr) ->
      begin match find_variable id with
      | None -> raise (ScopeError ("Affectation à une variable non déclarée : " ^ id))
      | Some { is_const = true; _ } ->
        raise (ScopeError ("Affectation interdite à une variable const : " ^ id))
      | _ -> check_expression expr
      end
    | Affectation (_, expr) -> check_expression expr
    | Par expr -> check_expression expr
    | Object content_list ->
      List.iter (fun (_, expr) -> check_expression expr) content_list
    | Array expr_list -> List.iter check_expression expr_list
  in

  (* Démarrer la vérification *)
  enter_scope ();
  List.iter check_body ast;
  exit_scope ();
  Printf.printf "Scope checking terminé sans erreurs.\n"

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    check_scope ast;
    (* Afficher l'AST *)
    Printf.printf "%s\n" (Ast.string_of_file ast)