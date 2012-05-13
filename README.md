# 4x4 Boggle Solver

An algorithm for quickly solving Boggle boards, written in OCaml. It 
references [this question on StackOverflow][1] : 

> Essentially, when the game starts you get a matrix of letters like so:
> 
>     F X I E
>     A M L O
>     E W B X
>     A S T U
>
> The goal of the game is to find as many words as you can that can be 
> formed by chaining letters together. You can start with any letter, 
> and all the letters that surround it are fair game, and then once 
> you move on to the next letter, all the letters that surround that 
> letter are fair game, except for any previously used letters. So in 
> the grid above, for example, I could come up with the words `LOB`, 
> `TUX`, `SEA`, `FAME`, etc. Words must be at least 3 characters,

  [1]: http://stackoverflow.com/questions/746082/

There were solutions in Python, Perl, Python, VB.NET and PHP, with 
performance between 1s and 30s. By comparison, the optimized OCaml 
version solves a board in 0.35ms.

## Solving Strategy

Searching for words in a Boggle board consists in tracing paths
in a graph with 16 node, thus enumerating all possible words on 
that Boggle board. The optimizations performed by this solver rely
on culling paths that do not match the provided dictionary of 
words. 

The first step is to turn the exploration of an arbitrary graph
into the exploration of a direct acyclic graph. This is done
by creating 16 copies of the board and connecting the nodes on
layer N to their adjacent nodes on layer N+1. For instance, in this
graph, cell M on layer 3 would point at cells I, J and N on layer 4. 

    A B C D
    E F G H 
    I J K L 
    M N O P

In theory, this directed acyclic graph is equivalent to the
original graph, because the same paths are possible (in terms of
ndoe labels encountered). In practice, this allows us to eliminate
edges based on two-letter sequence appearance statistics. Fewer
edges means a faster exploration. For example, the edge "GK" exists
between layers 2 and 3, but not between layers 0 and 1.

Once this initial step has been performed, the resulting graph
is explored using a trie structure to eliminate remaining edges 
based on the prefix path being explored. This means that if a 
path starts with letters that are not a prefix of any word, then
the exploration for that path will stop immediately. 

The exploration path returns a list of possible words of any length.
The cleanup path then sorts that list and eliminates words that
are shorter than three characters.

## Implementation

The dictionary file is preprocessed by the file `pairs.ml` which 
then generates: 

-   A (position,pair) table describing, for each of the 676 possible
    two-letter pairs, at which positions they may appear in words. 
    Pairs are encoded as an integer between 0 and 675 for faster
    access, and the list of possible positions are described as 
    a hard-coded function which returns `true` for every possible
    position.

-   The length of the longest possible word in the language (which
    is used to set the maximum size on the various buffers). This
    is 22 for my local dictionary, for instance.

-   A hard-coded trie structure, which is simply a tree where
    each node contains a boolean (is this node an end-of-word or
    not?) and each edge is labeled by a character. 

All of these are output as OCaml code to an intermediate file
`data.ml`. 

The `boggle.ml` file is the solver. It implements the algorithm
using the intermediate values from `data.ml`. This allows it to 
be extremely fast, as there is no runtime processing of 
dictionaries necessary. 
