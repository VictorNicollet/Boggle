(* Representing the board as a 16-character string. *)

let board = "FXIEAMLOEWBXASTU"

let print_board b = 
  Printf.printf "%c %c %c %c\n%c %c %c %c\n%c %c %c %c\n%c %c %c %c\n"
    b.[ 0] b.[ 1] b.[ 2] b.[ 3]
    b.[ 4] b.[ 5] b.[ 6] b.[ 7]
    b.[ 8] b.[ 9] b.[10] b.[11]
    b.[12] b.[13] b.[14] b.[15]
    
(* A representation of the board as a graph. This is not a simple 
   "connect adjacent cells" representation. The nodes are of the
   form (c,i) where c is the index of the cell in the board 
   (such as 12 for the bottom left corner) and i is the number of
   the letter in the word being build. So, for instance, the three
   potential moves from (12,3) are (8,4) (9,4) and (13,4). 

   The depth is represented implicitly, so the node that represents
   (12,3) is, in theory, [N [8;9;4]]. In practice, some of these
   edges may be eliminated because they carry impossible two-letter
   sequences. *)
type tree = N of tree list

