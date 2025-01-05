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

(* ----------------------- Question 4, check_types ----------------------- *)
exception TypeError of string

(* Environnement pour stocker les types des variables et fonctions *)
module Env = Map.Make(String)

(* Définition de l'environnement *)
type env = {
  variables : type_ Env.t; (* Types des variables *)
  return_type : type_ option; (* Type de retour de la fonction courante *)
}

(* Fonction utilitaire pour vérifier le sous-typage *)
let rec is_subtype t1 t2 =
  match (t1, t2) with
  | (_, Any) -> true (* Tout type est un sous-type de Any *)
  | (CteT (Decimal _), PrimitiveType Number)
  | (CteT (Binary _), PrimitiveType Number)
  | (CteT (Octal _), PrimitiveType Number)
  | (CteT (Hexa _), PrimitiveType Number)
  | (CteT (FloatDecimal _), PrimitiveType Number) -> true (* Les constantes numériques sont des sous-types de Number *)
  | (CteT (Boolean _), PrimitiveType Boolean) -> true  (* Les constantes booléennes sont des sous-types de Boolean *)
  | (CteT (String _), PrimitiveType String) -> true (* Les constantes chaînes sont des sous-types de String *)
  | (CteT c1, CteT c2) -> c1 = c2 (* Les constantes doivent être identiques *)
  | (PrimitiveType t1, PrimitiveType t2) -> t1 = t2 (* Les types primitifs doivent correspondre *)
  | (Union types, _) -> List.exists (fun t -> is_subtype t t2) types (* Un élément d'une union peut être un sous-type *)
  | (_, Union types) -> List.for_all (fun t -> is_subtype t1 t) types (* Tout élément doit être un sous-type *)
  | _ -> false

(* Vérification du typage des expressions *)
let rec check_expression env expr =
  match expr with
  | Par e -> check_expression env e  (* Vérification du type de l'expression entre parenthèses *)
  | Cte c -> (match c with
      | Decimal _ | Binary _ | Octal _ | Hexa _ | FloatDecimal _ -> PrimitiveType Number  (* Les constantes numériques sont de type Number *)
      | Boolean _ -> PrimitiveType Boolean (* Les constantes booléennes sont de type Boolean *)
      | String _ -> PrimitiveType String) (* Les constantes chaînes sont de type String *)
  | Left (IdLM id) -> (try Env.find id env.variables with Not_found -> raise (TypeError ("Variable non déclarée : " ^ id)))  (* Vérification d'une variable *)
  | Left (Table (e1, e2)) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      (match t1 with
       | ArrayT t -> if is_subtype t2 (PrimitiveType Number) then t else raise (TypeError "Index invalide")  (* Vérification d'un index valide *)
       | _ -> raise (TypeError "Accès à un type non tableau"))
  | Left (SubElement (e, id)) ->
      let t = check_expression env e in
      (match t with
       | ObjectT fields -> (try List.assoc id fields |> Option.get with Not_found -> raise (TypeError ("Champ non trouvé : " ^ id)))  (* Vérification d'un champ d'objet *)
       | Union types ->
          let field_types =
            List.filter_map
              (fun t -> match t with ObjectT fields -> List.assoc_opt id fields | _ -> None)
              types
          in
          if field_types = [] then raise (TypeError ("Champ non trouvé dans l'union : " ^ id))
          else Union (List.map Option.get field_types)  (* Union des types de champs *)
       | _ -> raise (TypeError ("Accès invalide au champ : " ^ id)))
  | Object fields -> ObjectT (List.map (fun (id, e) -> (id, Some (check_expression env e))) fields)  (* Vérification de la déclaration d'un objet *)
  | Array elems -> ArrayT (if elems = [] then Any else check_expression env (List.hd elems))  (* Vérification d'un tableau *)
  | Function (fn, args) ->
      let _ = check_expression env fn in (* Vérification de la fonction *)
      List.iter (fun arg -> ignore (check_expression env arg)) args;  (* Vérification des arguments *)
      Any (* Les fonctions renvoient Any par défaut *)
  | Unaire (_, e) -> check_expression env e (* Vérification d'une opération unaire *)
  | Bop (e1, op, e2) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      (match op with
       | Add ->
           if is_subtype t1 (PrimitiveType Number) && is_subtype t2 (PrimitiveType Number) then PrimitiveType Number  (* Addition numérique *)
           else if is_subtype t1 (PrimitiveType String) && is_subtype t2 (PrimitiveType String) then PrimitiveType String  (* Concatenation de chaînes *)
           else raise (TypeError "Opération '+' invalide")
       | _ ->
           if is_subtype t1 (PrimitiveType Number) && is_subtype t2 (PrimitiveType Number) then PrimitiveType Number  (* Autres opérations binaires numériques *)
           else raise (TypeError "Opération binaire invalide"))
  | Cond (e1, _, e2) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      if is_subtype t1 t2 || is_subtype t2 t1 then PrimitiveType Boolean  (* Vérification d'une condition *)
      else raise (TypeError "Condition invalide")
  | And (e1, e2) | Or (e1, e2) ->
      let _ = check_expression env e1 in
      let _ = check_expression env e2 in
      PrimitiveType Boolean (* Opérateurs logiques *)
  | Affectation (IdLM id, e) ->
      let t_var = (try Env.find id env.variables with Not_found -> Any) in
      let t_expr = check_expression env e in
      if is_subtype t_expr t_var then t_expr (* Vérification de l'affectation *)
      else raise (TypeError ("Incompatibilité de type pour " ^ id))
  | _ -> Any (* Par défaut, le type est Any *)

(* Vérification des instructions *)
let rec check_instruction env instr =
  match instr with
  | Empty -> env  (* Instruction vide *)
  | Expression e -> ignore (check_expression env e); env (* Ignorer le type d'une expression *)
  | Block body -> List.fold_left check_body env body (* Vérification d'un bloc *)
  | VarDeclaration bindings ->
      List.fold_left
        (fun env (Binding (id, t_opt, e_opt)) ->
           let t = match (t_opt, e_opt) with
             | Some t, Some e ->
                 let t_expr = check_expression env e in
                 if is_subtype t_expr t then t else raise (TypeError ("Incompatibilité de type pour " ^ id))
             | Some t, None -> t
             | None, Some e -> check_expression env e
             | None, None -> Any
           in
           { env with variables = Env.add id t env.variables })
        env bindings (* Ajout des variables à l'environnement *)
  | If (cond, i1, i2_opt) ->
      let _ = check_expression env cond in (* Vérification de la condition *)
      let env' = check_instruction env i1 in (* Vérification de l'instruction if *)
      (match i2_opt with Some i2 -> check_instruction env' i2 | None -> env')  (* Vérification de else *)
  | While (cond, i) -> let _ = check_expression env cond in check_instruction env i  (* Boucle While *)
  | Return e_opt ->
      (match (env.return_type, e_opt) with
       | Some t_ret, Some e ->
           let t_e = check_expression env e in
           if is_subtype t_e t_ret then env  (* Vérification du type de retour *)
           else raise (TypeError "Incompatibilité de type pour return")
       | Some Any, None -> env  (* Retour sans expression pour Any *)
       | None, _ -> raise (TypeError "Return en dehors d'une fonction")
       | _ -> env)

(* Vérification des déclarations et corps *)
and check_declaration env decl =
  match decl with
  | Alias (id, t) -> { env with variables = Env.add id t env.variables }  (* Alias de type *)
  | LocalLet bindings | LocalConst bindings ->
      List.fold_left
        (fun env (Binding (id, t_opt, e_opt)) ->
           let t = match (t_opt, e_opt) with
             | Some t, Some e ->
                 let t_expr = check_expression env e in
                 if is_subtype t_expr t then t else raise (TypeError ("Incompatibilité de type pour " ^ id))
             | Some t, None -> t
             | None, Some e -> check_expression env e
             | None, None -> Any
           in
           { env with variables = Env.add id t env.variables })
        env bindings (* Ajout des constantes ou variables locales à l'environnement *)
  | Function (id, bindings, ret_type, body) ->
      let env' =
        List.fold_left
          (fun env (Binding (id, t_opt, _)) ->
             let t = match t_opt with None -> Any | Some t -> t in
             { env with variables = Env.add id t env.variables })
          { env with return_type = (match ret_type with None -> Some Any | Some t -> Some t) }
          bindings
      in
      let _ = List.fold_left check_body env' body in (* Vérification du corps de la fonction *)
      env

and check_body env body =
  match body with
  | Declaration decl -> check_declaration env decl  (* Vérification d'une déclaration *)
  | Instr instr -> check_instruction env instr  (* Vérification d'une instruction *)

(* Vérification d'un fichier *)
let check_type file =
  let initial_env = { variables = Env.empty; return_type = None } in
  let _ = List.fold_left check_body initial_env file in (* Vérification de chaque élément du fichier *)
  Printf.printf "Type checking terminé sans erreurs.\n"

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    check_scope ast; (* Non fonctionnelle *)
    check_type ast; (* Non fonctionnelle *)
    (* Afficher l'AST *)
    Printf.printf "%s\n" (Ast.string_of_file ast)