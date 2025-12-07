

(* Exercice 3.2 *)

(* Définition des tokens *)


type token =
  (* Identifiers and numbers *)
  | Ident of string
  | Number of int

  (* Boolean constants *)
  | TRUE
  | FALSE

  (* Arithmetic operators *)
  | PLUS       (* + *)
  | MINUS      (* - *)
  | TIMES      (* * *)
  | DIV        (* / *)

  (* Boolean operators *)
  | AND        (* && *)
  | OR         (* || *)
  | NOT        (* ! *)

  (* Comparison operators *)
  | EQ         (* == *)
  | LT         (* < *)
  | LE         (* <= *)
  | GT         (* > *)
  | GE         (* >= *)

  (* Assignment and punctuation *)
  | ASSIGN     (* := *)
  | LPAREN     (* ( *)
  | RPAREN     (* ) *)
  | LBRACE     (* { *)
  | RBRACE     (* } *)
  | SEMI       (* ; *)

  | EOF



(* Analyseur lexical *)

let is_letter c =
  ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_'

let is_digit c =
  '0' <= c && c <= '9'

let rec skip_spaces = function
  | c::q when c = ' ' || c = '\n' || c = '\t' -> skip_spaces q
  | s -> s

let rec read_number acc = function
  | c::q when is_digit c ->
      let digit = int_of_char c - int_of_char '0' in
      read_number (acc * 10 + digit) q
  | s -> (acc, s)

let rec read_ident buf = function
  | c::q when is_letter c || is_digit c ->
      read_ident (buf ^ String.make 1 c) q
  | s -> (buf, s)

(* Lexer principal *)

let rec lex = function
  | [] -> (EOF, [])

  | s ->
      let s = skip_spaces s in
      match s with
      | [] -> (EOF, [])

      (* Identifiers or keywords *)
      | c::q when is_letter c ->
          let (id, rest) = read_ident (String.make 1 c) q in
          begin match id with
          | "true"  -> (TRUE, rest)
          | "false" -> (FALSE, rest)
          | _       -> (Ident id, rest)
          end

      (* Numbers *)
      | c::q when is_digit c ->
          let digit = int_of_char c - int_of_char '0' in
          let (n, rest) = read_number digit q in
          (Number n, rest)

      (* Two-character operators *)
      | ':'::'='::rest -> (ASSIGN, rest)
      | '='::'='::rest -> (EQ, rest)
      | '<'::'='::rest -> (LE, rest)
      | '>'::'='::rest -> (GE, rest)
      | '&'::'&'::rest -> (AND, rest)
      | '|'::'|'::rest -> (OR, rest)

      (* One-character tokens *)
      | '+'::rest -> (PLUS, rest)
      | '-'::rest -> (MINUS, rest)
      | '*'::rest -> (TIMES, rest)
      | '/'::rest -> (DIV, rest)
      | '!'::rest -> (NOT, rest)
      | '<'::rest -> (LT, rest)
      | '>'::rest -> (GT, rest)
      | '('::rest -> (LPAREN, rest)
      | ')'::rest -> (RPAREN, rest)
      | '{'::rest -> (LBRACE, rest)
      | '}'::rest -> (RBRACE, rest)
      | ';'::rest -> (SEMI, rest)

      | c::_ -> failwith (Printf.sprintf "Unknown char: %c" c)


(* tokenisation complet *)

let rec tokenize chars =
  match lex chars with
  | (EOF, _) -> [EOF]
  | (tok, rest) -> tok :: tokenize rest

let tokenize_string s =
  tokenize (List.init (String.length s) (String.get s))

(* Chainer lexicale -> syntaxe *)

let parse_from_string s =
  let toks = tokenize_string s in
  match parser_program toks with
  | Some (ast, []) -> ast
  | _ -> failwith "Syntax error"
