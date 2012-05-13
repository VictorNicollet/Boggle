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

  
