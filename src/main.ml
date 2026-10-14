open Lexing
open Parser
open Ast
open Lexer

(* Parse the file to create an AST *)
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


(* Scope checking *)
exception ScopeError of string

(* Definition of types to track variables *)
type variable_info = {
  mutable is_const: bool;  (* Indicates if the variable is declared as const *)
  mutable is_declared: bool;  (* Indicates if the variable has been declared *)
}

(* Environment stack to manage scope *)
type scope = (id, variable_info) Hashtbl.t
let new_scope () = Hashtbl.create 10

(* Main function *)
let check_scope (ast: file) =
  (* Environment stack *)
  let scope_stack = Stack.create () in

  (* Enter a new scope *)
  let enter_scope () = Stack.push (new_scope ()) scope_stack in
  (* Exit a scope *)
  let exit_scope () = ignore (Stack.pop scope_stack) in

  (* Look up a variable in scopes *)
  let find_variable id =
    let rec aux stack =
      if Stack.is_empty stack then None
      else
        let current_scope = Stack.top stack in
        match Hashtbl.find_opt current_scope id with
        | Some info -> Some info
        | None ->
          (* If the variable is not found in this scope, continue *)
          aux (Stack.copy stack |> fun s -> ignore (Stack.pop s); s)
    in
    aux scope_stack
  in

  (* Add a variable to the current scope *)
  let add_variable id is_const =
    match Stack.top_opt scope_stack with
    | Some current_scope ->
      Hashtbl.add current_scope id { is_const; is_declared = true }
    | None -> failwith "No active scope to declare a variable"
  in

  (* Recursive verification of a body *)
  let rec check_body body =
    match body with
    | Declaration decl -> check_declaration decl
    | Instr instr -> check_instruction instr

  (* Verification of a declaration *)
  and check_declaration = function
  | Alias (_, _) -> ()  (* No impact on identifiers *)
  | LocalLet bindings ->
      List.iter (fun (Binding (id, _, _)) -> add_variable id false) bindings
  | LocalConst bindings ->
      List.iter (fun (Binding (id, _, _)) -> add_variable id true) bindings
  | Function (id, bindings, _, body_list) ->
      (* Add the function itself to the scope before processing its parameters and body *)
      add_variable id false;  (* The function itself *)
      (* Enter a new scope for the function *)
      enter_scope ();
      List.iter (fun (Binding (id, _, _)) -> add_variable id false) bindings;  (* Add function parameters *)
      List.iter check_body body_list;  (* Check the function body *)
      exit_scope ()  (* Exit the function scope *)


  (* Verification of an instruction *)
  and check_instruction = function
  | Empty -> ()
  | Expression expr -> check_expression expr
  | Block body_list ->
    enter_scope ();
    List.iter check_body body_list;
    exit_scope ()
  | VarDeclaration bindings ->
    (* Immediately add variables to the scope for var *)
    List.iter (fun (Binding (id, _, expr_opt)) ->
      add_variable id false;  (* Immediate addition for var *)
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


  (* Verification of an expression *)
  and check_expression = function
    | Cte _ -> ()
    | Left left_Member -> ( match left_Member with
      | IdLM id ->
        begin match find_variable id with
        | None -> raise (ScopeError ("Variable used without declaration: " ^ id))
        | Some { is_declared = false; _ } ->
          raise (ScopeError ("Variable used before its declaration: " ^ id))
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
      | None -> raise (ScopeError ("Assignment to undeclared variable: " ^ id))
      | Some { is_const = true; _ } ->
        raise (ScopeError ("Forbidden assignment to const variable: " ^ id))
      | _ -> check_expression expr
      end
    | Affectation (_, expr) -> check_expression expr
    | Par expr -> check_expression expr
    | Object content_list ->
      List.iter (fun (_, expr) -> check_expression expr) content_list
    | Array expr_list -> List.iter check_expression expr_list
  in

  (* Start verification *)
  enter_scope ();
  List.iter check_body ast;
  exit_scope ();
  Printf.printf "Scope checking completed without errors.\n"

(* Type checking *)
exception TypeError of string

(* Environment to store types of variables and functions *)
module Env = Map.Make(String)

(* Definition of the environment *)
type env = {
  variables : type_ Env.t; (* Variable types *)
  return_type : type_ option; (* Return type of the current function *)
}

(* Utility function to check subtyping *)
let rec is_subtype t1 t2 =
  match (t1, t2) with
  | (_, Any) -> true (* Any type is a subtype of Any *)
  | (CteT (Decimal _), PrimitiveType Number)
  | (CteT (Binary _), PrimitiveType Number)
  | (CteT (Octal _), PrimitiveType Number)
  | (CteT (Hexa _), PrimitiveType Number)
  | (CteT (FloatDecimal _), PrimitiveType Number) -> true (* Numeric constants are subtypes of Number *)
  | (CteT (Boolean _), PrimitiveType Boolean) -> true  (* Boolean constants are subtypes of Boolean *)
  | (CteT (String _), PrimitiveType String) -> true (* String constants are subtypes of String *)
  | (CteT c1, CteT c2) -> c1 = c2 (* Constants must be identical *)
  | (PrimitiveType t1, PrimitiveType t2) -> t1 = t2 (* Primitive types must match *)
  | (Union types, _) -> List.exists (fun t -> is_subtype t t2) types (* An element of a union can be a subtype *)
  | (_, Union types) -> List.for_all (fun t -> is_subtype t1 t) types (* Every element must be a subtype *)
  | _ -> false

(* Verification of expression typing *)
let rec check_expression env expr =
  match expr with
  | Par e -> check_expression env e  (* Verification of parenthesized expression type *)
  | Cte c -> (match c with
      | Decimal _ | Binary _ | Octal _ | Hexa _ | FloatDecimal _ -> PrimitiveType Number  (* Numeric constants are of type Number *)
      | Boolean _ -> PrimitiveType Boolean (* Boolean constants are of type Boolean *)
      | String _ -> PrimitiveType String) (* String constants are of type String *)
  | Left (IdLM id) -> (try Env.find id env.variables with Not_found -> raise (TypeError ("Undeclared variable: " ^ id)))  (* Variable verification *)
  | Left (Table (e1, e2)) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      (match t1 with
       | ArrayT t -> if is_subtype t2 (PrimitiveType Number) then t else raise (TypeError "Invalid index")  (* Valid index verification *)
       | _ -> raise (TypeError "Access to a non-array type"))
  | Left (SubElement (e, id)) ->
      let t = check_expression env e in
      (match t with
       | ObjectT fields -> (try List.assoc id fields |> Option.get with Not_found -> raise (TypeError ("Field not found: " ^ id)))  (* Object field verification *)
       | Union types ->
          let field_types =
            List.filter_map
              (fun t -> match t with ObjectT fields -> List.assoc_opt id fields | _ -> None)
              types
          in
          if field_types = [] then raise (TypeError ("Field not found in union: " ^ id))
          else Union (List.map Option.get field_types)  (* Union of field types *)
       | _ -> raise (TypeError ("Invalid field access: " ^ id)))
  | Object fields -> ObjectT (List.map (fun (id, e) -> (id, Some (check_expression env e))) fields)  (* Object declaration verification *)
  | Array elems -> ArrayT (if elems = [] then Any else check_expression env (List.hd elems))  (* Array verification *)
  | Function (fn, args) ->
      let _ = check_expression env fn in (* Function verification *)
      List.iter (fun arg -> ignore (check_expression env arg)) args;  (* Arguments verification *)
      Any (* Functions return Any by default *)
  | Unaire (_, e) -> check_expression env e (* Unary operation verification *)
  | Bop (e1, op, e2) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      (match op with
       | Add ->
           if is_subtype t1 (PrimitiveType Number) && is_subtype t2 (PrimitiveType Number) then PrimitiveType Number  (* Numeric addition *)
           else if is_subtype t1 (PrimitiveType String) && is_subtype t2 (PrimitiveType String) then PrimitiveType String  (* String concatenation *)
           else raise (TypeError "Invalid '+' operation")
       | _ ->
           if is_subtype t1 (PrimitiveType Number) && is_subtype t2 (PrimitiveType Number) then PrimitiveType Number  (* Other numeric binary operations *)
           else raise (TypeError "Invalid binary operation"))
  | Cond (e1, _, e2) ->
      let t1 = check_expression env e1 in
      let t2 = check_expression env e2 in
      if is_subtype t1 t2 || is_subtype t2 t1 then PrimitiveType Boolean  (* Condition verification *)
      else raise (TypeError "Invalid condition")
  | And (e1, e2) | Or (e1, e2) ->
      let _ = check_expression env e1 in
      let _ = check_expression env e2 in
      PrimitiveType Boolean (* Logical operators *)
  | Affectation (IdLM id, e) ->
      let t_var = (try Env.find id env.variables with Not_found -> Any) in
      let t_expr = check_expression env e in
      if is_subtype t_expr t_var then t_expr (* Assignment verification *)
      else raise (TypeError ("Type mismatch for " ^ id))
  | _ -> Any (* Default type is Any *)

(* Verification of instructions *)
let rec check_instruction env instr =
  match instr with
  | Empty -> env  (* Empty instruction *)
  | Expression e -> ignore (check_expression env e); env (* Ignore expression type *)
  | Block body -> List.fold_left check_body env body (* Block verification *)
  | VarDeclaration bindings ->
      List.fold_left
        (fun env (Binding (id, t_opt, e_opt)) ->
           let t = match (t_opt, e_opt) with
             | Some t, Some e ->
                 let t_expr = check_expression env e in
                 if is_subtype t_expr t then t else raise (TypeError ("Type mismatch for " ^ id))
             | Some t, None -> t
             | None, Some e -> check_expression env e
             | None, None -> Any
           in
           { env with variables = Env.add id t env.variables })
        env bindings (* Add variables to environment *)
  | If (cond, i1, i2_opt) ->
      let _ = check_expression env cond in (* Condition verification *)
      let env' = check_instruction env i1 in (* If instruction verification *)
      (match i2_opt with Some i2 -> check_instruction env' i2 | None -> env')  (* Else verification *)
  | While (cond, i) -> let _ = check_expression env cond in check_instruction env i  (* While loop *)
  | Return e_opt ->
      (match (env.return_type, e_opt) with
       | Some t_ret, Some e ->
           let t_e = check_expression env e in
           if is_subtype t_e t_ret then env  (* Return type verification *)
           else raise (TypeError "Type mismatch for return")
       | Some Any, None -> env  (* Return without expression for Any *)
       | None, _ -> raise (TypeError "Return outside of a function")
       | _ -> env)

(* Verification of declarations and bodies *)
and check_declaration env decl =
  match decl with
  | Alias (id, t) -> { env with variables = Env.add id t env.variables }  (* Type alias *)
  | LocalLet bindings | LocalConst bindings ->
      List.fold_left
        (fun env (Binding (id, t_opt, e_opt)) ->
           let t = match (t_opt, e_opt) with
             | Some t, Some e ->
                 let t_expr = check_expression env e in
                 if is_subtype t_expr t then t else raise (TypeError ("Type mismatch for " ^ id))
             | Some t, None -> t
             | None, Some e -> check_expression env e
             | None, None -> Any
           in
           { env with variables = Env.add id t env.variables })
        env bindings (* Add local constants or variables to environment *)
  | Function (id, bindings, ret_type, body) ->
      let env' =
        List.fold_left
          (fun env (Binding (id, t_opt, _)) ->
             let t = match t_opt with None -> Any | Some t -> t in
             { env with variables = Env.add id t env.variables })
          { env with return_type = (match ret_type with None -> Some Any | Some t -> Some t) }
          bindings
      in
      let _ = List.fold_left check_body env' body in (* Function body verification *)
      env

and check_body env body =
  match body with
  | Declaration decl -> check_declaration env decl  (* Declaration verification *)
  | Instr instr -> check_instruction env instr  (* Instruction verification *)

(* File verification *)
let check_type file =
  let initial_env = { variables = Env.empty; return_type = None } in
  let _ = List.fold_left check_body initial_env file in (* Verification of each element in the file *)
  Printf.printf "Type checking completed without errors.\n"

let () =
  if Array.length Sys.argv <> 2 then
    Printf.eprintf "Usage: %s <file>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    let ast = parse_file filename in
    check_scope ast; (* Non-functional for variables declared with 'var' *)
    (*check_type ast;*) (* Non-functional *)
    print_endline (ast_to_js ast)