#use "anacomb.ml";;

(* Types pour représenter le programme *)
type variable = A | B | C | D

type expression =
  | Zero
  | One
  | Var of variable

type instruction =
  | Skip
  | Affectation of variable * expression
  | Sequence of instruction * instruction
  | If of variable * instruction * instruction
  | While of variable * instruction

type programme = instruction


(* Exercice 2.1.1 *)



(* Analyseur pour les variables *)
let analyse_variable = 
  terminal_res (function 
    | 'a' -> Some A
    | 'b' -> Some B 
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

(* Analyseur pour les expressions *)
let analyse_expression =
  (terminal '0' -+> epsilon_res Zero)
  +| (terminal '1' -+> epsilon_res One)
  +| (analyse_variable ++> fun v -> epsilon_res (Var v))

(* Analyseur pour les affectations *)
let analyse_affectation =
  analyse_variable ++> fun var ->
  terminal ':' -+>
  terminal '=' -+>
  analyse_expression ++> fun expr ->
  epsilon_res (Affectation(var, expr))

(* Analyseur pour l'instruction skip *)
let analyse_skip =
  terminal ';' -+> epsilon_res Skip

(* Analyseur récursif pour les instructions (déclaré plus tard) *)
let rec analyse_instruction l = analyse_instruction_impl l

(* Analyseur pour les instructions if *)
and analyse_if =
  terminal 'i' -+>
  terminal '(' -+>
  analyse_variable ++> fun cond ->
  terminal ')' -+>
  terminal '{' -+>
  analyse_instruction ++> fun alors ->
  terminal '}' -+>
  terminal '{' -+>
  analyse_instruction ++> fun sinon ->
  terminal '}' -+>
  epsilon_res (If(cond, alors, sinon))

(* Analyseur pour les instructions while *)
and analyse_while =
  terminal 'w' -+>
  terminal '(' -+>
  analyse_variable ++> fun cond ->
  terminal ')' -+>
  terminal '{' -+>
  analyse_instruction ++> fun corps ->
  terminal '}' -+>
  epsilon_res (While(cond, corps))

(* Analyseur pour une instruction simple (sans séquence) *)
and analyse_instruction_simple =
  analyse_skip
  +| analyse_affectation
  +| analyse_if
  +| analyse_while

(* Analyseur pour une instruction avec gestion des séquences *)
and analyse_instruction_impl =
  analyse_instruction_simple ++> fun instr1 ->
  (* Regarder s'il y a un ";" suivi d'une autre instruction *)
  (terminal ';' -+> analyse_instruction ++> fun instr2 ->
   epsilon_res (Sequence(instr1, instr2)))
  +| epsilon_res instr1

(* Analyseur pour un programme complet *)
let analyser_programme_complet =
  analyse_instruction ++> fun prog ->
  epsilon_res prog

(* Fonction utilitaire pour analyser une chaîne *)
let analyser_chaine chaine =
  try
    let input = list_of_string chaine in
    let resultat, reste = analyser_programme_complet input in
    if reste = [] then Some resultat else None
  with Echec -> None
