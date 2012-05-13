(* This file serves as a pre-processing step for the actual Boggle solver. 
   It reads strings from a dictionary and constructs two data structures
   that are then compiled into the solver: 

   - Several lists of letter pairs. List N contains all letter pairs that
     can appear at position [N..N+1] with any word in the target language.
     This is used to trim the solver graph. 
  
   - A trie structure that represents all possible words in the 
     language, flattened out in an array. 
*)

(* Dictionary source. Change this to handle other languages. *)
let dictionary   = "/usr/share/dict/american-english"

(* Determining whether a word is made only of latin characters (in order 
   to eliminate non-words that may appear in the dictionary, such as
   's or similar. This has the annoying side-effect that languages with
   accents will be missing words, so an initial accent removal phase
   (iconv-like) should be added for those languages. *)
let clean_word_regexp = Str.regexp "[a-z]+"
let clean_word word = 
  let word = String.lowercase word in
  let len  = String.length word in
  let matched = 
    Str.string_match clean_word_regexp word 0
    && Str.match_beginning () = 0 
    && Str.match_end () = len 
  in 
  if matched then Some word else None

(* This value initially loads all the words from the dictionary, 
   applying the [clean_word] function to eliminate non-words. *)
let clean_words = 
  let input = open_in dictionary in 
  let rec read acc = 
    let line = 
      try Some (input_line input) 
      with _ -> None 
    in
    match line with 
      | Some line -> let acc = 
		       match clean_word line with 
			 | None -> acc 
			 | Some word -> word :: acc
		     in	
		     read acc 
      | None -> acc
  in
  let list = read [] in
  close_in input ;
  list
  
(* Compute the maximum length of a word in the language. *)
let max_length = 
  List.fold_left 
    (fun length word -> max length (String.length word)) 0 
    clean_words

(* There are 676 = 26x26 possible two-letter pairs. We store 
   information about the distribution of such pairs in a 
   676-cell array, where 'bc' is stored as [bx26 + c]
   (that is, [1x26 + 2 = 28]). The function below takes two
   characters from a string, a position in that string, and
   returns the index of the corresponding two-letter pair. *)
let pair_index string pos = 
  let char_1 = Char.code string.[pos] - 97 in
  let char_2 = Char.code string.[succ pos] - 97 in
  char_1 * 26 + char_2 

(* We store information about how many pairs were encountered
   in an array-of-arrays, such that [stats.("ab").(i)] counts
   how many times the pair "ab" has been encountered at position
   [i..i+1] in a string. *)
let stats = Array.init 676 (fun _ -> Array.make (max_length - 1) 0)

(* Compute the stats for a single word. *)
let word_stats word = 
  for i = 0 to String.length word - 2 do 
    let j = pair_index word i in
    stats.(j).(i) <- succ stats.(j).(i) 
  done

(* Compute the stats for all words. *)
let () = List.iter word_stats clean_words

(* Transform the stats array into an array of "this pair can happen
   in this list of possible positions". *)
let appearances = Array.map 
  (fun array -> 
    List.map fst 
      (List.filter snd
	 (Array.to_list
	    (Array.mapi (fun i x -> i,x > 0) array))))
  stats

(* Generate the source code for the function that determines where
   a given character pair may appear. *)
module Definition = struct
  type t = int list 
  let compare = compare 
end

module DefinitionMap = Map.Make(Definition)

let appearance_source = 

  let defs, rev_appearances =
    Array.fold_left
      begin fun (defs,list) pos ->
	let var, defs = 
	  try DefinitionMap.find pos defs, defs 
	  with Not_found -> let var = DefinitionMap.cardinal defs in
			    var, DefinitionMap.add pos var defs
	in
	defs, var :: list
      end 
      (DefinitionMap.empty, [])
      appearances
  in

  let array_contents = 
    String.concat ";" 
      (List.rev_map (Printf.sprintf "_%d") rev_appearances) 
  in

  let definitions = 
    DefinitionMap.fold (fun list var acc ->
      (Printf.sprintf 
	 "  let _%d = function %s -> true | _ -> false in\n"
	 var
	 (String.concat " | " (List.map string_of_int list)))
      :: acc
    ) defs []
  in

  "let pairs = \n" 
  ^ String.concat "" definitions
  ^ "  [| "
  ^ array_contents 
  ^ " |]\n;;"
    
(* Extract the trie from the dictionary. The trie will be
   represented as a graph of valid prefixes, where each
   prefix contains a link to all its direct suffixes that 
   are still valid, and a boolean indicating whether the
   suffix is a word. *)
module Prefix = struct
  type t = string
  let compare = compare
end
  
module Graph = Map.Make(Prefix) 

(* Utility function: adding an edge to the graph. *)
let add_edge src dst graph = 
  let list, final = 
    try Graph.find src graph 
    with Not_found -> [], false
  in
  let list = dst :: list in
  Graph.add src (list,final) graph

(* Utility function: marking a node as final in the graph. *)
let make_final node graph = 
  let list, _ = 
    try Graph.find node graph 
    with Not_found -> [], false
  in
  Graph.add node (list,true) graph


    
