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
type tree = N of int * tree list

(* Adjacency list. Instead of writing down the rules, it's easier to 
   just write it by hand. *)
let adjacency = 
  [|
    [1;5;4] ;
    [0;2;6;5;4] ;
    [1;3;5;6;7] ;
    [2;6;7] ;
    [0;1;5;9;8] ;
    [0;1;2;4;6;8;9;10] ;
    [1;2;3;5;7;9;10;11] ;
    [2;3;6;10;11] ;
    [4;5;9;12;13] ;
    [4;5;6;8;10;12;13;14] ;
    [5;6;7;9;11;13;14;15] ;
    [6;7;10;14;15] ;
    [8;9;13] ;
    [8;9;10;12;14] ;
    [9;10;11;13;15] ;
    [10;11;14]
  |]

(* Constructing the full tree, up to the maximal depth allowed by 
   the language. This version IGNORES sequences of allowed characters.
*)
let make_tree b = 
  let all = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] in
  let rec aux depth node = 
    if depth = Data.longest then N (node,[]) else
      N (node, List.map (aux (succ depth)) adjacency.(node)) 
  in
  List.map (aux 0) all
