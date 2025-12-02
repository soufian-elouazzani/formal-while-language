#use "anacomb.ml";;

(* Types pour représenter WHILEb-- *)
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

(* ANALYSEUR AFFECTATION *)
let analyse_affectation =
  analyse_variable ++> fun var ->
  (terminal ':' --> terminal '=') -+>  (* consume ":=" *)
  analyse_expression ++> fun expr ->
    epsilon_res (Affectation(var, expr))


(* IF *)
let analyse_if = 
  (terminal 'i' --> terminal '(') -+> epsilon_res () ++> fun _ ->
  analyse_variable ++> fun cond ->
  (terminal ')' --> terminal '{') -+> epsilon_res () ++> fun _ ->
  (* inside the braces we parse a sequence *)
  (* not recursive left because sequence uses star combinator below *)
  (* we'll call analyse_sequence which is defined later; forward declare with "let rec" *)
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

(* --- Implementation réelle de sequence et instruction (remplacement des placeholders) --- *)

(* Nous définissons une fonction auxiliaire pour construire une Sequence en chaine *)
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
    if reste = [] then Some resultat else None
  with Echec -> None


(* --- Quelques tests rapides --- *)
let _ =
  match analyser_chaine "a:=1" with
  | Some (Affectation(A, One)) -> true
  | _ -> false

let _ =
  match analyser_chaine "a:=1;b:=0" with
  | Some (Sequence(Affectation(A, One), Affectation(B, Zero))) -> true
  | _ -> false

let _ =
  (* if with empty else *)
  match analyser_chaine "i(a){a:=1}{}" with
  | Some (If(A, Affectation(A, One), Skip)) -> true
  | _ -> false

let  _ =
  match analyser_chaine "" with
  | Some Skip -> true
  | _ -> false


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
