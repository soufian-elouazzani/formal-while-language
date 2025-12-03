#use "2_PartiePrincipale.ml"



let var_index = function
  | A -> 0
  | B -> 1
  | C -> 2
  | D -> 3

let lookup v st =
  List.nth st (var_index v)

let rec update_at i v = fun l ->
  match l with
  | [] -> []
  | _::q when i=0 -> v :: q
  | x::q -> x :: update_at (i-1) v q

let update v n st =
  update_at (var_index v) n st


let eval_expr e st =
  match e with
  | Zero -> 0
  | One -> 1
  | Var v -> lookup v st

(* SEMANTIQUE NATURELLE  *)

let rec exec instr st =
  match instr with
  | Skip -> st

  | Affectation (v,e) ->
      let n = eval_expr e st in
      update v n st

  | Sequence (i1,i2) ->
      let st1 = exec i1 st in
      exec i2 st1

  | If (v, then_i, else_i) ->
      if lookup v st = 1
      then exec then_i st
      else exec else_i st

  | While (v, body) ->
      if lookup v st = 0 then st
      else exec (While (v, body)) (exec body st)



let initial_state = [0;0;0;0]   (* a=0, b=0, c=0, d=0 *)


let print_state st =
  Printf.printf "a = %d\nb = %d\nc = %d\nd = %d\n"
    (List.nth st 0) (List.nth st 1)
    (List.nth st 2) (List.nth st 3)

let _ = analyser_chaine "a:=1"
    
let run s =
  match analyser_chaine s with
  | None ->
      print_endline "Erreur d'analyse syntaxique."
  | Some ast ->
      let final_st = exec ast initial_state in
      print_state final_st



let () =
  run "w(a){i(b){c:=1}{c:=0}}"
