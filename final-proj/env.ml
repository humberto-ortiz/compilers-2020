(* Temporary file for addapting env to support local vars and arguments. *)

open Printf

type reg = 
  | RSP
  | RBP                         (* Stack pointer *)
  | RAX
  | RBX

type arg = 
  | Const of int64
  | Reg of reg
  | RegOffset of reg * int (* RegOffset(reg, i) represents address [reg + 8*i] *)

(* This has to change to support local vars and arguments !! *)
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

(* 
let add name env =
  let slot = 1 + (List.length env) in
  ((name,slot)::env, slot)
*)

