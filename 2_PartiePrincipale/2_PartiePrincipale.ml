#use "anacomb.ml";;


type variable = A | B | C | D

type expression =
  | Zero
  | One 
  | Var of variable
  | Not of expression
  | And of expression * expression
  | Or  of expression * expression

type instruction =
  | Skip
  | Affectation of variable * expression
  | Sequence of instruction * instruction
  | If of variable * instruction * instruction
  | While of variable * instruction

type programme = instruction

(* on consume les espaces avant chaque charactere on utilisant skip_blanks *)

(* Consume any number of spaces, tabs, or newlines *)
let rec skip_blanks lst =
  match lst with
  | (' ' | '\t' | '\n' | '\r') :: q -> skip_blanks q
  | _ -> lst

(* Same parser combinator as terminal *)
let token c =
  fun lst ->
    match skip_blanks lst with
    | x :: q when x = c -> ([c], q)
    | _ -> raise Echec

(* aussi pour terminal_res *)
let token_res f =
  fun lst ->
    match skip_blanks lst with
    | x :: q ->
        (match f x with
         | Some v -> (v, q)
         | None -> raise Echec)
    | _ -> raise Echec

(* Analyseur pour les variables *)
let analyse_variable = 
  token_res (function 
    | 'a' -> Some A
    | 'b' -> Some B 
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)
(* pour la langage whileb--
(* Analyseur pour les expressions *)
let analyse_expression =
  (terminal '0' -+> epsilon_res Zero)
  +| (terminal '1' -+> epsilon_res One) 
  +| (analyse_variable ++> fun v -> epsilon_res (Var v))

*)

(* pour la langage whileb *)

let rec analyse_F lst =
  (
    (* '!' F *)
    (terminal '!' -+> analyse_F ++> fun e -> epsilon_res (Not e))

    +|
    (* '(' E ')' *)
    (terminal '(' -+> analyse_E ++> fun e ->
       terminal ')' -+> epsilon_res e)

    +|
    (* variables ou constantes *)
    analyse_atom
  ) lst

and analyse_atom lst =
  (
    (terminal '0' -+> epsilon_res Zero)
    +|
    (terminal '1' -+> epsilon_res One)
    +|
    (analyse_variable ++> fun v -> epsilon_res (Var v))
  ) lst

and analyse_T lst =
  (
    analyse_F ++> fun f1 ->
    star_list (terminal '.' -+> analyse_F) ++> fun rest ->
      let e =
        List.fold_left (fun acc f -> And(acc,f)) f1 rest
      in
      epsilon_res e
  ) lst

and analyse_E lst =
  (
    analyse_T ++> fun t1 ->
    star_list (terminal '+' -+> analyse_T) ++> fun rest ->
      let e =
        List.fold_left (fun acc t -> Or(acc,t)) t1 rest
      in
      epsilon_res e
  ) lst

(* ANALYSEUR AFFECTATION *)
let analyse_affectation =
  analyse_variable ++> fun var ->
  (terminal ':' --> terminal '=') -+>  (* consume ":=" *)
  analyse_E ++> fun expr ->
    epsilon_res (Affectation(var, expr))

(* IF *)
let analyse_if = 
  (terminal 'i' --> terminal '(') -+> epsilon_res () ++> fun _ ->
  analyse_variable ++> fun cond ->
  (terminal ')' --> terminal '{') -+> epsilon_res () ++> fun _ ->
  failwith "placeholder_if"

(* WHILE *)
let analyse_while =
  (terminal 'w' --> terminal '(') -+> epsilon_res () ++> fun _ ->
  analyse_variable ++> fun cond ->
  (terminal ')' --> terminal '{') -+> epsilon_res () ++> fun _ ->
  failwith "placeholder_while"

(* Forward declaration (to be updated later) *)
let rec analyse_instruction =
  failwith "placeholder_instr"

and analyse_sequence =
  failwith "placeholder_seq"


let build_sequence_from_list = function
  | [] -> Skip
  | [i] -> i
  | i :: rest ->
      List.fold_left (fun acc ins -> Sequence(acc, ins)) i rest

(* Mutually recursive parsers *)
let rec analyse_instruction =
  fun lst ->
    (
      analyse_affectation
      +|
      ( (terminal 'i' --> terminal '(') -+> epsilon_res () ++> fun _ ->
        analyse_variable ++> fun cond ->
        (terminal ')' --> terminal '{') -+> epsilon_res () ++> fun _ ->
        analyse_sequence ++> fun alors ->
        (terminal '}' --> terminal '{') -+> epsilon_res () ++> fun _ ->
        analyse_sequence ++> fun sinon ->
        terminal '}' -+> epsilon_res () ++> fun _ ->
        epsilon_res (If(cond, alors, sinon))
      )
      +|
      ( (terminal 'w' --> terminal '(') -+> epsilon_res () ++> fun _ ->
        analyse_variable ++> fun cond ->
        (terminal ')' --> terminal '{') -+> epsilon_res () ++> fun _ ->
        analyse_sequence ++> fun corps ->
        terminal '}' -+> epsilon_res () ++> fun _ ->
        epsilon_res (While(cond, corps))
      )
    ) lst

and analyse_sequence lst =
  (
    (
      analyse_instruction ++> fun first ->
      star_list (terminal ';' -+> analyse_instruction) ++> fun rest ->
      let seq = first :: rest in
      epsilon_res (build_sequence_from_list seq)
    )
    +|
    epsilon_res Skip
  ) lst

(* Analyseur principal : le programme est une séquence *)
let analyser_programme = analyse_sequence

(* Fonction pour analyser une string *)
let analyser_chaine chaine =
  try
    let chars = list_of_string chaine in
    let resultat, reste = analyser_programme chars in
    Some resultat
  with Echec -> None


(* Exercice 2.1.2 *)

let test p expected =
  match analyser_chaine p with
  | Some ast ->
      if ast = expected then
        print_endline ("OK: " ^ p)
      else
        print_endline ("FAIL (wrong AST): " ^ p)
  | None ->
      print_endline ("FAIL (parse error): " ^ p)
;;

(* ------- TEST PROGRAMS ------- *)

(* 1 — simple affectation *)
let () =
  test "a:=1"
    (Affectation (A, One))

(* 2 — sequence of affectations *)
let () =
  test "a:=1;b:=0"
    (Sequence (Affectation(A,One), Affectation(B,Zero)))

(* 3 — simple IF *)
let () =
  test "i(a){b:=1}{b:=0}"
    (If (A, Affectation(B,One), Affectation(B,Zero)))

(* 4 — simple WHILE *)
let () =
  test "w(a){b:=1}"
    (While (A, Affectation(B,One)))

(* 5 — nested IF in WHILE *)
let () =
  test "w(a){i(b){c:=1}{c:=0}}"
    (While (A,
       If (B, Affectation(C,One), Affectation(C,Zero))
    ))

(* 6 — nested WHILE in IF *)
let () =
  test "i(a){w(b){c:=1}}{c:=0}"
    (If (A,
         While (B, Affectation(C,One)),
         Affectation(C,Zero)
    ))

let () =
  test "a:=a+!b.1+(c.d)"
    (Affectation(A,
       Or(
         Or(Var A, And(Not(Var B), One)),
         And(Var C, Var D)
       )
    ))



(* Exercice 2.2.1 *)

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

(* old while eval 
let eval_expr e st =
  match e with
  | Zero -> 0
  | One -> 1
  | Var v -> lookup v st
*)

let rec eval_expr e st =
  match e with
  | Zero -> 0
  | One -> 1
  | Var v -> lookup v st

  | Not e1 ->
      if eval_expr e1 st = 1 then 0 else 1

  | And (e1, e2) ->
      if eval_expr e1 st = 1 && eval_expr e2 st = 1
      then 1 else 0

  | Or (e1, e2) ->
      if eval_expr e1 st = 1 || eval_expr e2 st = 1
      then 1 else 0

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



let initial_state = [1;1;0;0]   (* a=0, b=0, c=0, d=0 *)


let print_state st =
  Printf.printf "a = %d\nb = %d\nc = %d\nd = %d\n"
    (List.nth st 0) (List.nth st 1)
    (List.nth st 2) (List.nth st 3)

    
let run s initial_state=
  match analyser_chaine s with
  | None ->
      print_endline "Erreur d'analyse syntaxique."
  | Some ast ->
      let final_st = exec ast initial_state in
      print_state final_st

 
let () =
  run "w(a){i(b){c:=0;a:=0}{c:=1;a:=0}}" [1;1;1;0]

let () =
  run "w(a){i(b){c:=0;a:=0}{c:=1;a:=0}}" [1;0;0;0]














(* DEBUGER printer *)
let string_of_var = function A -> "A" | B -> "B" | C -> "C" | D -> "D"

let rec string_of_expr = function
  | Zero -> "Zero"
  | One  -> "One"
  | Var v -> "Var(" ^ string_of_var v ^ ")"
  | Not e -> "Not(" ^ string_of_expr e ^ ")"
  | And (e1,e2) -> "And(" ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"
  | Or  (e1,e2) -> "Or("  ^ string_of_expr e1 ^ ", " ^ string_of_expr e2 ^ ")"

let rec string_of_instr = function
  | Skip -> "Skip"
  | Affectation(v,e) ->
      "Affectation(" ^ string_of_var v ^ ", " ^ string_of_expr e ^ ")"
  | Sequence(i1,i2) ->
      "Sequence(" ^ string_of_instr i1 ^ ", " ^ string_of_instr i2 ^ ")"
  | If(v,i1,i2) ->
      "If(" ^ string_of_var v ^ ", " ^ string_of_instr i1 ^ ", " ^ string_of_instr i2 ^ ")"
  | While(v,i) ->
      "While(" ^ string_of_var v ^ ", " ^ string_of_instr i ^ ")"

let string_of_charlist cl =
  String.concat "" (List.map (String.make 1) cl)

(* helper that prints analyser_programme result including leftover *)
let debug_parse_and_print s =
  let chars = list_of_string s in
  try
    let ast, rest = analyser_programme chars in
    Printf.printf "INPUT: %s\n" s;
    Printf.printf "PARSED AST: %s\n" (string_of_instr ast);
    Printf.printf "REMAINDER (%d chars): \"%s\"\n"
      (List.length rest) (string_of_charlist rest)
  with
  | Echec -> Printf.printf "INPUT: %s\nParse failed (Echec)\n" s

<<<<<<< HEAD
(* run the debug for a failing example *)
=======
>>>>>>> 3d7f18d (Fixing the README file)
let () = debug_parse_and_print "a:=a+!b.1+(c.d)"











