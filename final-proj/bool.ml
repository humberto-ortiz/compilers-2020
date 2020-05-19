open Printf
open Int64
open Syntax

type reg = 
  | RSP
  | RBP                         (* Stack pointer *)
  | RAX
  | RBX

let reg_to_string reg =
  match reg with
  | RSP -> "RSP"
  | RBP -> "RBP"
  | RAX -> "RAX"
  | RBX -> "RBX"

type arg = 
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

let arg_to_string arg =
  match arg with
  | Const n -> Int64.to_string n
  | Reg reg -> reg_to_string reg
  | RegOffset (reg, off) -> "[" ^ (reg_to_string reg) ^ " + 8*" ^ (string_of_int off) ^ "]"

type instruction = 
  | IJl of string
  | IJg of string
  | IJle of string
  | IJge of string
  | IJne of string
  | IMov of arg * arg
  | IAdd of arg * arg
  | ICmp of arg * arg
  | IJe of string
  | ILabel of string
  | IJump of string
  | ISub of arg * arg
  | IMul of arg 
  | ISar of arg * arg
  | IAnd of arg * arg
  | IOr of arg * arg
  | IXor of arg * arg
  | ICall of string
  | IPush of arg
  | IPop of arg
  | IRet

let rec asm_to_string (asm : instruction list) : string =
  match asm with
  | [] -> ""
  | ISub (arg1, arg2)::tail -> "sub " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IMov (arg1, arg2)::tail -> "mov " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAdd (arg1, arg2)::tail -> "add "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | ICmp (arg1, arg2)::tail -> "cmp " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IJe label::tail -> "je " ^ label ^ "\n" ^ asm_to_string tail
  | ILabel label::tail -> label ^ ":\n" ^ asm_to_string tail
  | IJump label::tail -> "jmp " ^ label ^ "\n" ^ asm_to_string tail
  | IMul (arg1)::tail -> "mul " ^  arg_to_string arg1 ^ "\n" ^ asm_to_string tail
  | ISar (arg1, arg2)::tail -> "sar " ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IAnd (arg1, arg2)::tail -> "and "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IOr (arg1, arg2)::tail -> "or "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IXor (arg1, arg2)::tail -> "xor "  ^ arg_to_string arg1 ^ ", " ^ arg_to_string arg2 ^ "\n" ^ asm_to_string tail
  | IJle label::tail -> "jle " ^ label ^ "\n" ^ asm_to_string tail
  | IJg label::tail -> "jg " ^ label ^ "\n" ^ asm_to_string tail
  | IJl label::tail -> "jl " ^ label ^ "\n" ^ asm_to_string tail
  | IJge label::tail -> "jge " ^ label ^ "\n" ^ asm_to_string tail
  | IJne label::tail -> "jne " ^ label ^ "\n" ^ asm_to_string tail
  | ICall name::tail -> "call " ^ name ^ "\n" ^ asm_to_string tail
  | IPush arg::tail -> "push " ^ arg_to_string arg ^ "\n" ^ asm_to_string tail
  | IPop arg::tail -> "pop " ^ arg_to_string arg ^ "\n" ^ asm_to_string tail
  | IRet :: tail -> "ret\n" ^ asm_to_string tail

type env = (string * arg) list

let rec lookup (name : string) (env : env) : arg =
  match env with
  | [] -> failwith (sprintf "Identifier %s not found in environment" name)
  | (n, arg)::rest ->
     if name = n then arg else (lookup name rest)

let is_local_var (arg : arg) : bool =
  match arg with
  | RegOffset (reg, _) ->
    if reg = RSP then true else false
  | _ -> false

let rec count_local_vars_in_env (env : env) : int =
  match env with
  | [] -> 0
  | (_, arg)::rest -> 
    if (is_local_var arg) then (count_local_vars_in_env rest) + 1
    else count_local_vars_in_env rest

(*
This function would need to be modified in the future if something else other 
than local vars and func args are added to the env. i.e, if we add another register
other than RSP and RBP to the reg in RegOffsets in the env, then modify this function.
*)
let count_func_args_in_env (env : env) : int =
    let total = List.length env in
    let local_vars_count = count_local_vars_in_env env in
    total - local_vars_count

let add name env reg =
  if reg = RSP then
    let local_vars_count = 1 + (count_local_vars_in_env env) in (*The 1+ is to offset the 0 in case there are no vars in env.*)
    let arg = RegOffset(reg, ~-1 * local_vars_count) in
    ((name, arg)::env, arg)
  else 
    let args_in_env_count = 1 + (count_func_args_in_env env) in
    let arg = RegOffset(reg, 1 * args_in_env_count) in
    ((name, arg)::env, arg)

let gensym =
  let counter = ref 0 in
  (fun basename ->
    counter := !counter + 1;
    sprintf "%s_%d" basename !counter);;

let is_imm e =
  match e with
  | Num _ -> true
  | Id _ -> true
  | _ -> false

let rec is_anf e =
  match e with
  | Add1 e -> is_imm e
  | Sub1 e -> is_imm e
  | BinOp (e1, _, e2) -> is_imm e1 && is_imm e2
  | Let (_, e1, e2) -> is_anf e1 && is_anf e2
  | If (cond, thn, els) -> is_imm cond && is_anf thn && is_anf els
  | _ -> is_imm e 

let rec anf_v1 e =
  match e with
  | Add1 e1 ->
    let (e1_ans, e1_context) = anf_v1 e1 in
    let temp = gensym "add1" in
    (Id(temp), (* the answer *)
     e1_context @ (* the context needed for the left answer to make sense *)
     [(temp, Add1(e1_ans))]) (* definition of the answer *)

  | Sub1 e1 ->
    let (e1_ans, e1_context) = anf_v1 e1 in
    let temp = gensym "sub1" in
    (Id(temp), (* the answer *)
     e1_context @ (* the context needed for the left answer to make sense *)
     [(temp, Sub1(e1_ans))]) (* definition of the answer *)

  | BinOp (e1, op, e2) ->
     let (left_ans, left_context) = anf_v1 e1 in
     let (right_ans, right_context) = anf_v1 e2 in
     let temp = gensym "binop" in
       (Id(temp), left_context @ right_context @ [(temp, BinOp (left_ans, op, right_ans))])

  | Num _ -> (e, [])

  | If (cond, thn, els) ->
      let(cond_ans,cond_context) = anf_v1 cond in 
      let (thn_ans, thn_context) = anf_v1 thn in
      let (els_ans, els_context) = anf_v1 els in
      let cond_temp = gensym "cond" in
      (Let (cond_temp,cond_ans, If(Id cond_temp,thn_ans,els_ans)),
        thn_context @ els_context @cond_context)

  | Let (id,e2,e3) ->
      let (id_ans, id_context) = anf_v1 e2 in
      let (env_ans, env_context) = anf_v1 e3 in
      let temp = gensym "let" in
      (Id(temp), id_context @ env_context  @ [(temp, Let (id, id_ans, env_ans))])

let rec anf_helper e context =
  match context with
  | [] -> e
  | (id, e2) :: tail -> Let (id, e2, anf_helper e tail)

let anf (e : expr) : expr =
  let (e, context ) = anf_v1 e in
    anf_helper e context

(* Should this block be in the syntax.ml file? *)
let min_cobra_int = Int64.div Int64.min_int 2L
let max_cobra_int = Int64.div Int64.max_int 2L
let const_true  =   0xFFFFFFFFFFFFFFFFL
let const_false =   0x7FFFFFFFFFFFFFFFL
let const_bool_mask=0x8000000000000000L

(* compile is responsible for compiling just a single expression,
   and does not care about the surrounding scaffolding *)
let rec compile_expr (e : expr) (env : env) : instruction list =
  match e with
  | Num n -> if n > max_cobra_int || n < min_cobra_int then failwith ("Integer overflow " ^ (Int64.to_string n))
   else
      [ IMov(Reg(RAX), Const(Int64.shift_left n 1)) ]

  | Bool true -> (*valor cierto*) [ IMov(Reg(RAX), Const(const_true)) ]

  | Bool false -> (*valor falso*) [ IMov(Reg(RAX), Const(const_false)) ]

  | Add1 e -> (compile_expr e env) @ [ IAdd(Reg(RAX), Const (Int64.shift_left 1L 1)) ]  

  | Sub1 e -> (compile_expr e env) @ [ IAdd(Reg(RAX), Const (-2L)) ]

  | BinOp (e1, Plus, e2) ->
     (compile_expr e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile_expr e2 env) @
     [ IAdd (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)

  | BinOp (e1, Minus, e2) ->
     (compile_expr e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile_expr e2 env) @
     [ ISub (Reg(RAX), Reg(RBX))]   (* restar - cuidado con orden argumentos *)  

  | BinOp (e1, Mult, e2) ->
     (compile_expr e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile_expr e2 env) @
     [ IMul (Reg(RBX))]   (* muliplicar - cuidado con orden argumentos *)
     @[ISar (Reg(RAX), Const(1L))]

  | BinOp (e1, And, e2) ->
     (compile_expr e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile_expr e2 env) @
     [ IAnd (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)

  | BinOp (e1, Or, e2) ->
     (compile_expr e1 env) @
     [ IMov (Reg(RBX), Reg(RAX))] @ (* guardarlo *)
     (compile_expr e2 env) @
     [ IOr (Reg(RAX), Reg(RBX))]   (* sumar - cuidado con orden argumentos *)

  | Not e -> [IMov(Reg(RBX), Const(const_bool_mask))] @ (compile_expr e env) @ [ IXor(Reg(RAX), Reg(RBX)) ]

  | Id name -> let regOffSet = (lookup name env) in
               [ IMov(Reg(RAX), regOffSet) ]
 
  | Let (x, e, b) ->
     let (env', regOffSet) = add x env RSP in
     (* Compile the binding, and get the result into RAX *)
     (compile_expr e env)
     (* Copy the result in RAX into the appropriate stack slot *)
     @ [ IMov(regOffSet, Reg(RAX)) ]
     (* Compile the body, given that x is in the correct slot when it's needed *)
     @ (compile_expr b env')

  | If (e1, e2, e3) ->
     let else_label = gensym "else_branch" in
     let done_label = gensym "done" in
     compile_expr e1 env
     @ [ ICmp (Reg(RAX), Const(0L)) ;
         IJe (else_label) ] @
       compile_expr e2 env
       @ [ IJump done_label ]
       @ [ ILabel else_label ]
       @ compile_expr e3 env
       @ [ ILabel done_label ]

  | BinOp (e1, Less, e2) ->
     let less_label = gensym "less" in
     let great_label = gensym "greater" in
     let done_label = gensym "done" in
     compile_expr e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJl (less_label) ] @
       [ILabel great_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel less_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, LessEq, e2) ->
     let lessEq_label = gensym "lessEq" in
     let great_label = gensym "greater" in
     let done_label = gensym "done" in
     compile_expr e2 env   
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJle (lessEq_label) ] @
       [ILabel great_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel lessEq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Greater, e2) ->
     let less_label = gensym "less" in
     let greater_label = gensym "greater" in
     let done_label = gensym "done" in
     compile_expr e2 env   
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJg (greater_label) ] @
       [ILabel less_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel greater_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, GreaterEq, e2) ->
     let less_label = gensym "less" in
     let greaterEq_label = gensym "greaterEq" in
     let done_label = gensym "done" in
     compile_expr e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJge (greaterEq_label) ] @
       [ILabel less_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel greaterEq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Eq, e2) ->
     let eq_label = gensym "Equal" in
     let ne_label = gensym "NotEqual" in
     let done_label = gensym "done" in
     compile_expr e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJe (eq_label) ] @
       [ILabel ne_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel eq_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ]

  | BinOp (e1, Ne, e2) ->
     let eq_label = gensym "Equal" in
     let ne_label = gensym "NotEqual" in
     let done_label = gensym "done" in
     compile_expr e2 env
     @[IMov (Reg(RBX) , Reg(RAX))]
     @compile_expr e1 env
     @ [ ICmp (Reg(RAX), Reg(RBX)) ;
         IJne (ne_label) ] @
       [ILabel eq_label; IMov (Reg(RAX),Const(const_false))]
       @ [ IJump done_label ]
       @ [ ILabel ne_label ]
       @ [IMov (Reg(RAX),Const(const_true))]
       @ [ ILabel done_label ] 

  | FuncCall (name, args) ->
     let argsCount = List.length args in
     let compiled_args_list_of_list = List.rev_map (fun arg -> (compile_expr arg env) @ [IPush(Reg RAX)]) args in
     let compiled_and_pushed_args = List.flatten compiled_args_list_of_list in  
     compiled_and_pushed_args
     @ [ICall name]
     @ [IAdd((Reg RSP), Const(Int64.of_int (8*argsCount)))] (* Clean the stack from the args. *)

(* Uncomment this and comment the FuncCall above to make test with 1 arg.
  | FuncCall (name, [arg]) ->
     compile_expr arg env
     @ [ IPush (Reg RAX) ]
     @ [ICall name]
     @ [IAdd((Reg RSP), Const(Int64.of_int (8*1)))] *)
     
let rec countVarsHelper (exp : expr) (counter : int) : int =
  match exp with
  | Num _ -> counter
  | Id _ -> counter
  | Bool _ -> counter
  | Add1 exp2 -> countVarsHelper exp2 counter
  | Sub1 exp2 -> countVarsHelper exp2 counter
  | Not exp2 -> countVarsHelper exp2 counter
  | If (exp2, exp3, exp4) -> countVarsHelper exp4 ( countVarsHelper exp3 (countVarsHelper exp2 counter) )
  | BinOp (left_exp, _, right_exp) -> countVarsHelper right_exp ( countVarsHelper left_exp counter ) 
  | Let (_, exp2, exp3) -> countVarsHelper exp3 ( countVarsHelper exp2 (counter + 1) )

let countVars = fun (exp : expr) : int ->
  let count = countVarsHelper exp 0 in
  count

let compile_decl (decl : decl) : instruction list =
  match decl with
  | Func (name, args, body) -> 
      let varsCount = countVars body in
      let env = List.mapi (fun index arg -> (arg, RegOffset(RBP, index+1))) args in
      [ILabel name]
      (* Prologue *)
      @ [IPush(Reg RBP)]                                        (* Save caller's stack frame address (return address). *)
      @ [IMov((Reg RBP), (Reg RSP))]                            (* Create new stack frame. *)
      @ [ISub((Reg RSP), (Const (Int64.of_int (8*varsCount))))] (* Allocate space in stack for local vars. *)
      (* Body *)
      @ compile_expr body env                                   (* Compile body of the function. *)
      (* Epilogue *)
      @ [IMov((Reg RSP), (Reg RBP))]                            (* Destroy the previously created stack frame. *)
      @ [IPop(Reg RBP)]                                         (* Get caller's stack frame address (return address. *)
      @ [IRet]

let rec compile_decls (decls : decl list) (instructions : instruction list) : instruction list =
  match decls with
  | [] -> instructions
  | decl :: decls2 -> 
      let instructions2 = compile_decl decl in
      let combined_instructions = instructions2 @ instructions in
      compile_decls decls2 combined_instructions

let compile = fun (prog : prog) : (instruction list * instruction list) ->
  match prog with
  | Expr exp -> ([], compile_expr exp [])
  | Decls (decls, exp) -> 
      let decl_instructions = compile_decls decls [] in
      let exp_instructions = compile_expr exp [] in
      (decl_instructions, exp_instructions)


(* compile_prog surrounds a compiled program by whatever scaffolding is needed *)
let compile_prog (prog : prog) : string =
  (* compile the program *)
  let (decl_instrs, exp_instrs) = compile prog in (* Descomponener el tuplo y poner las funciones en .text y la expr en asm_string. *)
  (* convert it to a textual form *)
  let asm_exp_string = asm_to_string exp_instrs in
  let asm_decl_string = asm_to_string decl_instrs in
  (* surround it with the necessary scaffolding *)
  let prelude = "
section .text
" ^ asm_decl_string ^ "
global our_code_starts_here
our_code_starts_here:" in
  let suffix = "ret" in
  prelude ^ "\n" ^ asm_exp_string ^ "\n" ^ suffix
;;
