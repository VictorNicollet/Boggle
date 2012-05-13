(* Representing the board as a 16-character string. *)

let board = String.lowercase "FXIEAMLOEWBXASTU"

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
   the language. Only allows edges that respect the allowed
   two-character sequences.

   It uses memoization so that building the tree is a linear operation
   instead of exponential (like a graph traversal would be). 
*)
let make_tree b = 
  let all = [0;1;2;3;4;5;6;7;8;9;10;11;12;13;14;15] in
  let memo = Array.init Data.longest (fun _ -> Array.make 16 None) in

  let adjacent depth node = 
    let c1 = (Char.code b.[node] - 97) * 26 in
    List.filter begin fun i -> 
      let seq = Char.code b.[i] - 97 + c1 in
      Data.pairs.(seq) depth
    end adjacency.(node)
  in

  let rec aux depth node = 
    if depth = Data.longest then N (node,[]) else
      match memo.(depth).(node) with 
	| None -> 
	  let list = List.map (aux (succ depth)) (adjacent depth node) in
	  let tree = N (node, list) in 
	  memo.(depth).(node) <- Some tree ; tree
	| Some tree -> tree
  in
  List.map (aux 0) all

(* In order to avoid moving through the same cell more than once, 
   have an object that registers when the cell is entered or left. *)
let visitor () = 
  let visited = Array.make 16 false in
  fun node if_visited if_not_visited ->
    if visited.(node) then Lazy.force if_visited else
      ( visited.(node) <- true ;
	let inner = Lazy.force if_not_visited in
	visited.(node) <- false ;
	inner ) 

(* Quick preview: traverse the tree and print all maximum words that are, in 
   theory, allowed by the two-character sequence analysis. *)
    
let rec all_strings board treelist = 
  let visit = visitor () in
  let rec aux acc treelist = 
    List.iter begin fun (N (node, treelist)) ->
      visit node 
	(lazy ())
	(lazy (
	  let acc = acc ^ String.make 1 board.[node] in
	  if treelist = [] then print_endline acc else
	    aux acc treelist
	 ))
    end treelist
  in
  aux "" treelist
    
