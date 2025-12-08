type variable = int
  
type aexp =
  | Aco of int              (* Aco : nat -> aexp *)
  | Ava of variable         (* Ava : nat -> aexp *)
  | Apl of aexp * aexp      (* Apl : aexp -> aexp -> aexp *)
  | Amu of aexp * aexp      (* Amu : aexp -> aexp -> aexp *)
  | Amo of aexp * aexp      (* Amo : aexp -> aexp -> aexp *)


type bexp =
  | Btrue
  | Bfalse
  | Bnot of bexp
  | Band of bexp * bexp
  | Bor  of bexp * bexp
  | Beq  of bexp * bexp     
  | Beqnat of aexp * aexp   

module type STATE_SIG = sig
  type t
  val init : t
  val get : variable -> t -> int
  val update : variable -> int -> t -> t
end

module ListState : STATE_SIG = struct
  type t = int list
      
  let init = [] (*initial state*)

  (*get var*)
  let rec get id s = 
    match id, s with
    | 0, v :: _ -> v
    | n, _ :: q -> get (n - 1) q
    | _, [] -> 0

  (*update var*)
  let rec update i v = fun l ->
    match i, l with
    | 0, _ :: q -> v :: q                  
    | 0, []     -> [v]                     
    | n, x :: q -> x :: (update (n-1) v q) 
    | n, []     -> 0 :: (update (n-1) v []) 
end

let rec evalA expr st =
  match expr with
  | Aco n -> n
  | Ava v -> ListState.get v st
  | Apl (a1, a2) -> (evalA a1 st) + (evalA a2 st)
  | Amu (a1, a2) -> (evalA a1 st) * (evalA a2 st)
  | Amo (a1, a2) -> (evalA a1 st) - (evalA a2 st)


let rec evalB b s =
  match b with
  | Btrue -> true
  | Bfalse -> false
  | Bnot b' -> not (evalB b' s)
  | Band (b1, b2) -> (evalB b1 s) && (evalB b2 s)
  | Bor (b1, b2) -> (evalB b1 s) || (evalB b2 s)
  | Beq (b1, b2) -> (evalB b1 s) = (evalB b2 s)
  | Beqnat (n1, n2) -> (evalA n1 s) = (evalA n2 s)

type winstr =
  | Skip
  | Assign of variable * aexp
  | Seq    of winstr * winstr
  | If     of bexp * winstr * winstr
  | While  of bexp * winstr
type programme = winstr

type config =
  | Inter of winstr * ListState.t 
  | Final of ListState.t

let rec faire_un_pas (p : programme) (s : ListState.t) : config =
  match p with
  | Skip -> Final s
  | Assign (x, a) -> 
      let val_a = evalA a s in
      Final (ListState.update x val_a s)
  | Seq (i1, i2) -> 
      (match faire_un_pas i1 s with
       | Final s' -> Inter (i2, s')          
       | Inter (i1', s') -> Inter (Seq(i1', i2), s') 
      )
  | If (b, i1, i2) ->
      if evalB b s then Inter (i1, s) else Inter (i2, s)
  | While (b, i) ->
    Inter (If (b, Seq (i, While (b, i)), Skip), s)


let debug (prog : programme) : ListState.t =
  print_endline "\n=== DEBUG MODE ===";
  print_endline "Press ENTER to execute the next step.";

  let rec loop (p : programme) (s : ListState.t) (step : int) =
    (*did a "translator" to see what's happening*)
    Printf.printf "\n[STEP %d] Current Instruction: " step;
    (match p with
     | Skip -> Printf.printf "Skip"
     | Assign (id, _) -> Printf.printf "Assignment Var[%d]" id
     | Seq (i1, _) ->                                         (*finally fixed the SEQ*)
        Printf.printf "Sequence [Executing: ";
        (match i1 with
         | Assign (id, _) -> Printf.printf "Assign Var[%d]" id
         | Skip -> Printf.printf "Skip"
         | If _ -> Printf.printf "If"
         | While _ -> Printf.printf "While"
         | Seq _ -> Printf.printf "Sequence"
        );
        Printf.printf "]";
     | If _ -> Printf.printf "If / Else Condition"
     | While _ -> Printf.printf "While Loop"
    );
    print_newline ();

    Printf.printf "  State: Var[0]=%d | Var[1]=%d | Var[2]=%d\n" (ListState.get 0 s) (ListState.get 1 s) (ListState.get 2 s);

    let _ = read_line() in

    match faire_un_pas p s with
    | Final s_final ->
        print_endline "--- FINISHED ---";
        Printf.printf "  State: Var[0]=%d | Var[1]=%d | Var[2]=%d\n" (ListState.get 0 s_final) (ListState.get 1 s_final) (ListState.get 2 s_final);
        s_final
        
    | Inter (next_p, next_s) ->
        loop next_p next_s (step + 1)
  in
  loop prog ListState.init 1
    
(*  Var[0] := 3; While (Var[0] != 0) ... *)
let prog1 = Seq(
  Assign(0, Aco 3),
  While(
    Bnot(Beqnat(Ava 0, Aco 0)),
    Assign(0, Amo(Ava 0, Aco 1))
  )
)

let s1 = debug prog1
let () = assert (ListState.get 0 s1 = 0)
  
let prog2 = Seq(
  Assign(0, Aco 10),              (* Var[0] := 10 *)
  If(
    Beqnat(Ava 0, Aco 10),        (* If Var[0] == 10 *)
    Assign(1, Aco 99),            (* THEN: Var[1] := 99 *)
    Assign(1, Aco 0)              (* ELSE: Var[1] := 0 *)
  )
)
let s2 = debug prog2
let () = assert (ListState.get 0 s2 = 10)
let () = assert (ListState.get 1 s2 = 99)

let prog3 = Seq(
  Assign(0, Aco 2),               (* Var[0] := 2 *)
  Seq(
    Assign(1, Aco 3),             (* Var[1] := 3 *)
    Assign(2, Amu(Ava 0, Ava 1))  (* Var[2] := Var[0] * Var[1] *)
  )
)

let s3 = debug prog3
let () = assert (ListState.get 0 s3 = 2)
let () = assert (ListState.get 1 s3 = 3)
let () = assert (ListState.get 2 s3 = 6)
