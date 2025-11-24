
(*
GRAMMAR:
   V = 'a' | 'b' | 'c' | 'd'
   C = 0 | 1 | V
   A = V ":=" C
   W = "w" "(" V ")" "{" S1 "}"
   I = "i" "(" V ")" "{" S1 "}" "{" S1 "}"
   INSTR = A | W | I
   S1 = INSTR S2 | epsilon
   S2 = ";" INSTR S2 | epsilon

*)
