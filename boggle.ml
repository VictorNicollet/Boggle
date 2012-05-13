(* Representing the board as a 16-character string. *)

let board = "FXIEAMLOEWBXASTU"

let print_board b = 
  Printf.printf "%c %c %c %c\n%c %c %c %c\n%c %c %c %c\n%c %c %c %c\n"
    b.[ 0] b.[ 1] b.[ 2] b.[ 3]
    b.[ 4] b.[ 5] b.[ 6] b.[ 7]
    b.[ 8] b.[ 9] b.[10] b.[11]
    b.[12] b.[13] b.[14] b.[15]
    

