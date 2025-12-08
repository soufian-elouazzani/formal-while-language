(* ==================================================== *)
(* Exercice 3.2 — Analyse lexicale + analyse syntaxique *)
(* ==================================================== *)

(* ----------------------- *)
(* definitions des TOKEN   *)
(* ----------------------- *)

type token =
  | INT of int
  | IDENT of string

  | TRUE
  | FALSE

  | PLUS
  | MINUS
  | TIMES
  | DIV

  | AND
  | OR
  | NOT

  | EQ
  | LT
  | LE
  | GT
  | GE

  | LPAREN
  | RPAREN
  | EOF


(* --------- *)
(*  LEXER    *)
(* --------- *)

let is_letter c =
  ('a' <= c && c <= 'z')
  || ('A' <= c && c <= 'Z')
  || c = '_'

let is_digit c = ('0' <= c && c <= '9')

let rec skip_spaces = function
  | (' ' | '\n' | '\t') :: q -> skip_spaces q
  | s -> s


let rec read_number acc = function
  | c :: q when is_digit c ->
      let d = int_of_char c - int_of_char '0' in
      read_number (acc * 10 + d) q
  | rest -> (acc, rest)

let rec read_ident buf = function
  | c :: q when is_letter c || is_digit c ->
      read_ident (buf ^ String.make 1 c) q
  | rest -> (buf, rest)


let rec lex chars =
  let chars = skip_spaces chars in
  match chars with
  | [] -> (EOF, [])

  | c :: q when is_letter c ->
      let (id, rest) = read_ident (String.make 1 c) q in
      begin match id with
      | "true"  -> (TRUE, rest)
      | "false" -> (FALSE, rest)
      | _       -> (IDENT id, rest)
      end

  | c :: q when is_digit c ->
      let d = int_of_char c - int_of_char '0' in
      let (n, rest) = read_number d q in
      (INT n, rest)

  | '=' :: '=' :: rest -> (EQ, rest)
  | '<' :: '=' :: rest -> (LE, rest)
  | '>' :: '=' :: rest -> (GE, rest)
  | '&' :: '&' :: rest -> (AND, rest)
  | '|' :: '|' :: rest -> (OR, rest)

  | '+' :: rest -> (PLUS, rest)
  | '-' :: rest -> (MINUS, rest)
  | '*' :: rest -> (TIMES, rest)
  | '/' :: rest -> (DIV, rest)
  | '!' :: rest -> (NOT, rest)
  | '<' :: rest -> (LT, rest)
  | '>' :: rest -> (GT, rest)
  | '(' :: rest -> (LPAREN, rest)
  | ')' :: rest -> (RPAREN, rest)

  | c :: _ -> failwith ("Unknown character: " ^ String.make 1 c)


let rec tokenize chars =
  match lex chars with
  | (EOF, _) -> [EOF]
  | (tok, rest) -> tok :: tokenize rest

let tokenize_string s =
  tokenize (List.init (String.length s) (String.get s))


(* ---------------- *)
(* AST              *)
(* ---------------- *)

type expr =
  | EInt of int
  | EVar of string
  | Plus of expr * expr
  | Minus of expr * expr
  | Times of expr * expr
  | Div of expr * expr
  | And of expr * expr
  | Or of expr * expr
  | Not of expr
  | Eq of expr * expr
  | Lt of expr * expr
  | Le of expr * expr
  | Gt of expr * expr
  | Ge of expr * expr


(* --------- *)
(*  PARSER   *)
(* --------- *)

let expect tok = function
  | t :: rest when t = tok -> rest
  | _ -> failwith "Unexpected token"


let rec parse_expr toks = parse_or toks

and parse_or toks =
  let left, toks = parse_and toks in
  match toks with
  | OR :: rest ->
      let right, rest2 = parse_or rest in
      (Or(left, right), rest2)
  | _ -> (left, toks)

and parse_and toks =
  let left, toks = parse_cmp toks in
  match toks with
  | AND :: rest ->
      let right, rest2 = parse_and rest in
      (And(left, right), rest2)
  | _ -> (left, toks)

and parse_cmp toks =
  let left, toks = parse_add toks in
  match toks with
  | EQ :: rest ->
      let r, rest2 = parse_add rest in
      (Eq(left, r), rest2)
  | LT :: rest ->
      let r, rest2 = parse_add rest in
      (Lt(left, r), rest2)
  | LE :: rest ->
      let r, rest2 = parse_add rest in
      (Le(left, r), rest2)
  | GT :: rest ->
      let r, rest2 = parse_add rest in
      (Gt(left, r), rest2)
  | GE :: rest ->
      let r, rest2 = parse_add rest in
      (Ge(left, r), rest2)
  | _ -> (left, toks)

and parse_add toks =
  let rec aux left toks =
    match toks with
    | PLUS :: rest ->
        let right, rest2 = parse_mul rest in
        aux (Plus(left, right)) rest2
    | MINUS :: rest ->
        let right, rest2 = parse_mul rest in
        aux (Minus(left, right)) rest2
    | _ -> (left, toks)
  in
  let left, toks = parse_mul toks in
  aux left toks

and parse_mul toks =
  let rec aux left toks =
    match toks with
    | TIMES :: rest ->
        let right, rest2 = parse_unary rest in
        aux (Times(left, right)) rest2
    | DIV :: rest ->
        let right, rest2 = parse_unary rest in
        aux (Div(left, right)) rest2
    | _ -> (left, toks)
  in
  let left, toks = parse_unary toks in
  aux left toks

and parse_unary toks =
  match toks with
  | NOT :: rest ->
      let e, rest2 = parse_unary rest in
      (Not e, rest2)
  | MINUS :: rest ->
      let e, rest2 = parse_unary rest in
      (Minus (EInt 0, e), rest2)
  | _ -> parse_atom toks

and parse_atom toks =
  match toks with
  | INT n :: rest -> (EInt n, rest)
  | IDENT s :: rest -> (EVar s, rest)
  | LPAREN :: rest ->
      let e, rest2 = parse_expr rest in
      begin match rest2 with
      | RPAREN :: rest3 -> (e, rest3)
      | _ -> failwith "Missing ')'"
      end
  | _ -> failwith "Unexpected token in expression"


(* -------------------------------- *)
(* L'entree principale              *)
(* -------------------------------- *)

let parser_program s =
  let toks = tokenize_string s in
  let ast, rest = parse_expr toks in
  match rest with
  | [EOF] | [] -> ast
  | _ -> failwith "Syntax error after expression"


(* --------------- *)
(* TESTS           *)
(* --------------- *)

let rec show_expr e =
  match e with
  | EInt n -> string_of_int n
  | EVar s -> s
  | Plus (a,b) -> "(" ^ show_expr a ^ " + " ^ show_expr b ^ ")"
  | Minus (a,b) -> "(" ^ show_expr a ^ " - " ^ show_expr b ^ ")"
  | Times (a,b) -> "(" ^ show_expr a ^ " * " ^ show_expr b ^ ")"
  | Div (a,b) -> "(" ^ show_expr a ^ " / " ^ show_expr b ^ ")"
  | Eq (a,b) -> "(" ^ show_expr a ^ " = " ^ show_expr b ^ ")"
  | Lt (a,b) -> "(" ^ show_expr a ^ " < " ^ show_expr b ^ ")"
  | Le (a,b) -> "(" ^ show_expr a ^ " <= " ^ show_expr b ^ ")"
  | Gt (a,b) -> "(" ^ show_expr a ^ " > " ^ show_expr b ^ ")"
  | Ge (a,b) -> "(" ^ show_expr a ^ " >= " ^ show_expr b ^ ")"
  | And (a,b) -> "(" ^ show_expr a ^ " and " ^ show_expr b ^ ")"
  | Or  (a,b) -> "(" ^ show_expr a ^ " or " ^ show_expr b ^ ")"
  | Not e -> "(not " ^ show_expr e ^ ")"

module Ast = struct
  let show = show_expr
end


let run_test name input =
  try
    let ast = parser_program input in
    Printf.printf "Test: %s\nInput: %s\n-> OK\nAST: %s\n\n"
      name input (Ast.show ast)
  with
  | Failure msg ->
      Printf.printf "Test: %s\nInput: %s\n-> ERROR: %s\n\n"
        name input msg
  | e ->
      Printf.printf "Test: %s\nInput: %s\n-> EXCEPTION: %s\n\n"
        name input (Printexc.to_string e)

let () =
  Printf.printf "==================== RUNNING TESTS ====================\n";

  (* simple tests *)
  run_test "integer" "42";
  run_test "variable" "x";
  run_test "addition" "1 + 2";
  run_test "precedence" "1 + 2 * 3";
  run_test "parentheses" "(1 + 2) * 3";

  (* boolean ops *)
  run_test "and/or" "a and b or c";

  (* comparisons *)
  run_test "comparisons" "x < 10";


  Printf.printf "====================== DONE TESTS ======================\n";


