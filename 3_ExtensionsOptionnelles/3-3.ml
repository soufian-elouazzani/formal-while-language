#use "3-2.ml"

(* Exercise 3.3 *)

(* Utilities - lazy list type *)
type 'a lazylist = unit -> 'a contents
and 'a contents = Nil | Cons of 'a * 'a lazylist

exception Echec

(* Convert an eager list to a lazylist *)
let rec lazy_of_list l () =
  match l with
  | [] -> Nil
  | x :: xs -> Cons (x, lazy_of_list xs)

(* Convert lazylist to eager list (forces it). Useful for debugging only. *)
let rec list_of_lazy ll =
  match ll () with
  | Nil -> []
  | Cons (x, xs) -> x :: list_of_lazy xs

(* Convert string to eager char list*)
let list_of_string s =
  let n = String.length s in
  let rec boucle i =
    if i = n then [] else s.[i] :: boucle (i+1)
  in boucle 0

(* lazy char stream from string *)
let lazy_of_string s = lazy_of_list (list_of_string s)


type 'term analist = 'term lazylist -> 'term lazylist

(* The ranalist returns a result and the remaining lazylist *)
type ('res, 'term) ranalist = 'term lazylist -> 'res * 'term lazylist


(* ----------------------- primitives (lazy) ----------------------- *)

(* terminal constant *)
let terminal (c : 't) : 't analist =
  fun ll ->
    match ll () with
    | Cons (x, xs) when x = c -> xs
    | _ -> raise Echec

(* terminal conditionnel *)
let terminal_cond (p : 't -> bool) : 't analist =
  fun ll ->
    match ll () with
    | Cons (x, xs) when p x -> xs
    | _ -> raise Echec

(* epsilon *)
let epsilon : 't analist = fun l -> l

(* sequence a1 --> a2 *)
let (-->) (a1 : 'term analist) (a2 : 'term analist) : 'term analist =
  fun l ->
    let l' = a1 l in
    a2 l'

(* alternative a1 -| a2 *)
let (-|) (a1 : 'term analist) (a2 : 'term analist) : 'term analist =
  fun l -> try a1 l with Echec -> a2 l

(* repetition (Kleene star) *)
let rec star (a : 'term analist) : 'term analist = fun l ->
  l |> ((a --> star a) -| epsilon)

(* -------------------- ranalist primitives (lazy) -------------------- *)

(* epsilon_res *)
let epsilon_res (info : 'res) : ('res, 'term) ranalist =
  fun l -> (info, l)

(* terminal_res *)
let terminal_res (f : 'term -> 'res option) : ('res, 'term) ranalist =
  fun ll ->
    match ll () with
    | Cons (x, xs) ->
        (match f x with
         | Some y -> (y, xs)
         | None -> raise Echec)
    | Nil -> raise Echec

(* a1 -+> a2 : analist followed by ranalist *)
let ( -+> ) (a1 : 'term analist) (a2 : ('res, 'term) ranalist) :
      ('res, 'term) ranalist =
  fun l ->
    let l' = a1 l in
    a2 l'

(* a1 ++> f : ranalist followed by function producing ranalist *)
let (++>) (a1 : ('resa, 'term) ranalist) (a2 : 'resa -> ('resb, 'term) ranalist) :
      ('resb, 'term) ranalist =
  fun l ->
    let (x, l') = a1 l in
    a2 x l'

(* alternative between ranalist parsers *)
let (+|) (a1 : ('res, 'term) ranalist) (a2 : ('res, 'term) ranalist) :
      ('res, 'term) ranalist =
  fun l ->
    try a1 l with Echec -> a2 l

(* -------------------- helpers from original anacomb -------------------- *)

let (<<) f g = fun x -> f (g x)
let (>>) f g = fun x -> g (f x)

let rec star_pipe_R2L (a : ('r -> 'r, 'term) ranalist) : ('r -> 'r, 'term) ranalist =
  let rec a_star = fun l ->
    ( ( a ++> fun f -> a_star ++> fun f_star -> epsilon_res (f << f_star) )
      +|
        epsilon_res (fun x -> x)
    ) l
  in a_star

let star_R2L (a : ('r -> 'r, 'term) ranalist) (r0 : 'r) : ('r, 'term) ranalist =
  star_pipe_R2L a ++> fun f -> epsilon_res (f r0)

let star_list (a : ('a, 'term) ranalist) : ('a list, 'term) ranalist =
  star_R2L (a ++> fun x -> epsilon_res (fun l -> x :: l)) []

let rec star_pipe_L2R (a : ('r -> 'r, 'term) ranalist) : ('r -> 'r, 'term) ranalist =
  let rec a_star = fun l ->
    ( ( a ++> fun f -> a_star ++> fun f_star -> epsilon_res (f >> f_star) )
      +|
        epsilon_res (fun x -> x)
    ) l
  in a_star

let star_L2R (r0 : 'r) (a : ('r -> 'r, 'term) ranalist) : ('r, 'term) ranalist =
  star_pipe_L2R a ++> fun f -> epsilon_res (r0 |> f)






(* -------------------- *)
(*        TESTS         *)
(* -------------------- *)

let print_expr e =
  let rec aux = function
    | EInt n -> Printf.sprintf "Int(%d)" n
    | EVar s -> Printf.sprintf "Var(%s)" s
    | Plus(a,b)  -> Printf.sprintf "Plus(%s,%s)" (aux a) (aux b)
    | Minus(a,b) -> Printf.sprintf "Minus(%s,%s)" (aux a) (aux b)
    | Times(a,b) -> Printf.sprintf "Times(%s,%s)" (aux a) (aux b)
    | Div(a,b)   -> Printf.sprintf "Div(%s,%s)"   (aux a) (aux b)
    | And(a,b)   -> Printf.sprintf "And(%s,%s)"   (aux a) (aux b)
    | Or(a,b)    -> Printf.sprintf "Or(%s,%s)"    (aux a) (aux b)
    | Not e      -> Printf.sprintf "Not(%s)"      (aux e)
    | Eq(a,b)    -> Printf.sprintf "Eq(%s,%s)"    (aux a) (aux b)
    | Lt(a,b)    -> Printf.sprintf "Lt(%s,%s)"    (aux a) (aux b)
    | Le(a,b)    -> Printf.sprintf "Le(%s,%s)"    (aux a) (aux b)
    | Gt(a,b)    -> Printf.sprintf "Gt(%s,%s)"    (aux a) (aux b)
    | Ge(a,b)    -> Printf.sprintf "Ge(%s,%s)"    (aux a) (aux b)
  in
  print_endline (aux e)


let run_test s =
  Printf.printf "\nTEST: %s\n" s;
  try
    let ast = parser_program s in
    print_expr ast
  with _ ->
    print_endline "!! PARSE ERROR !!"


let () =
  print_endline "\n===================";
  print_endline "  RUNNING TESTS";
  print_endline "===================";

  (* Arithmetic *)
  run_test "1+2";
  run_test "1+2*3";
  run_test "(1+2)*3";


  (* Mix *)
  run_test "1+2==3";
  run_test "1<2 && 3<4";
  run_test "!false";
  run_test "x + 3 * y";


  print_endline "\nDone.\n";
