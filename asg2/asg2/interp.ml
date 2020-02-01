(* $Id: interp.ml,v 1.8 2020-01-24 11:42:24-08 - - $ *)

open Absyn

exception Unimplemented of string
let no_expr reason = raise (Unimplemented reason)
let no_stmt reason continuation = raise (Unimplemented reason)

let want_dump = ref false

let rec eval_expr (expr : Absyn.expr) : float = match expr with
    | Number number -> number
    | Memref memref -> eval_memref memref
    | Unary (oper, expr) -> eval_unary oper expr
    | Binary (oper, expr1, expr2) -> eval_binary oper expr1 expr2

and eval_memref (reference : Absyn.memref) =
    match reference with
    | Variable i -> Hashtbl.find Tables.variable_table i

and eval_unary (oper : Absyn.oper) (expr : Absyn.expr) =
    match expr with
    | Memref ref -> 
      Hashtbl.find Tables.unary_fn_table oper (eval_expr expr)
    | Number one -> Hashtbl.find Tables.unary_fn_table oper one
    | _ -> Hashtbl.find Tables.unary_fn_table oper (eval_expr expr)

and eval_binary (oper : Absyn.oper) (expr1 : Absyn.expr) 
                (expr2 : Absyn.expr) =
    match expr1, expr2 with
    | Number one, Number two -> 
      Hashtbl.find Tables.binary_fn_table oper one two
    | _, _ -> Hashtbl.find Tables.binary_fn_table oper 
      (eval_expr expr1) (eval_expr expr2)

let rec interpret (program : Absyn.program) = match program with
    | [] -> ()
    | firstline::continuation -> match firstline with
      | _, _, None -> interpret continuation
      | _, _, Some stmt -> (interp_stmt stmt continuation)

and interp_stmt (stmt : Absyn.stmt) (continuation : Absyn.program) =
    match stmt with
    | Dim (ident, expr) -> interp_dim ident expr continuation
    | Let (memref, expr) -> interp_let memref expr continuation
    | Goto label -> interp_goto label continuation
    | If (expr, label) -> interp_if expr label continuation
    | Print print_list -> interp_print print_list continuation
    | Input memref_list -> interp_input memref_list continuation

and interp_dim (ident : Absyn.ident) (expr : Absyn.expr) 
               (continuation : Absyn.program) =
    match expr with
    | Number one -> Hashtbl.add Tables.array_table ident 
        (Array.create_float (int_of_float one));
    interpret continuation

and interp_let (memref : Absyn.memref) (expr : Absyn.expr) 
               (continuation : Absyn.program) =
    match memref with
    | Variable x -> 
      Hashtbl.add Tables.variable_table x (eval_expr expr);
    interpret continuation

and interp_goto (label : Absyn.label) (continuation : Absyn.program) =
    interpret (Hashtbl.find Tables.label_table label)

and interp_if (expr : Absyn.expr) (label : Absyn.label) 
              (continuation : Absyn.program) =
    match expr with
    | Binary (oper, expr1, expr2) when oper = "=" -> 
      if (eval_expr expr1) = (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation
    | Binary (oper, expr1, expr2) when oper = "<" -> 
      if (eval_expr expr1) < (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation
    | Binary (oper, expr1, expr2) when oper = ">" -> 
      if (eval_expr expr1) > (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation
    | Binary (oper, expr1, expr2) when oper = "!=" -> 
      if (eval_expr expr1) <> (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation
    | Binary (oper, expr1, expr2) when oper = ">=" -> 
      if (eval_expr expr1) >= (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation
    | Binary (oper, expr1, expr2) when oper = "<=" -> 
      if (eval_expr expr1) <= (eval_expr expr2) 
      then interp_goto label continuation else interpret continuation

and interp_print (print_list : Absyn.printable list)
                 (continuation : Absyn.program) =
    let print_item item =
        (print_string " ";
         match item with
         | String string ->
           let regex = Str.regexp "\"\\(.*\\)\""
           in print_string (Str.replace_first regex "\\1" string)
         | Printexpr expr ->
           print_float (eval_expr expr))
    in (List.iter print_item print_list; print_newline ());
    interpret continuation

and interp_input (memref_list : Absyn.memref list)
                 (continuation : Absyn.program)  =
    let input_number memref =
        try  let number = Etc.read_number ()
             in match memref with Variable i -> 
             Hashtbl.add Tables.variable_table i number
        with End_of_file -> 
             Hashtbl.add Tables.variable_table "eof" 1.0
    in List.iter input_number memref_list;
    interpret continuation

let interpret_program program =
    (Tables.init_label_table program; 
     if !want_dump then Tables.dump_label_table ();
     if !want_dump then Dumper.dump_program program;
     interpret program)

