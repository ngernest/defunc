# Defunctionalization at Work

The code from *Defunctionalization at Work* (Danvy & Nielsen, 2001), 
translated from Standard ML to OCaml. 

- To compile, run `dune build` (tested with OCaml 4.13.1).

A note from Section 1.5 of the paper:
> All the programs we consider perform a recursive descent and use an auxiliary function. 
> - When this auxiliary function is higher-order, defunctionalization yields a first-order version with an accumulator 
>   (e.g., tree flattening & list reversal)
> - When this auxiliary function is first-order, we transform it into CPS; defunctionalization then yields an iterative   
>   first-order version with an accumulator in the form of a data structure 
>   (e.g. string parsing & regex matching)