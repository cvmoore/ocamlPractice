(*Returns the last element of a list *)
let rec last list = 
    match list with
    [h] -> Some h
    |
    h::t -> last t
    |
    [] -> None

(*Returns the last two elements of a list *)
let rec last_two list = 
    match list with
    [h] -> None
    |
    [h;h1] -> Some (h, h1)
    |
    h::t -> last_two t
    |
    [] -> None

(*Find the Kth element of a list *)
let rec at num list = 
    match list with
    [h] -> if num=1 then Some h else None
    | 
    [] -> None
    |
    h::t -> if num=1 then Some h else at (num-1) t

(*Returns the length of a list*)
let rec length list =
    match list with 
    [] -> 0
    |
    h::t -> 1 + (length t)

(*Reverses a list*)
let reverse list =
    let rec aux accum list = 
        match list with 
        [] -> accum
        |
        h::t -> aux (h::accum) t
    in aux [] list

let is_palindrome list =
    list=(List.rev list)

(* There is no nested list type in OCaml, so we need to define one
     first. A node of a nested list is either an element, or a list of
     nodes. *)
type 'a node =
    | One of 'a 
    | Many of 'a node list

(*Flatten a nested list structure *)
let flatten str =
    let rec aux acc str =
        match str with
        [] -> acc
        |
        One h::t -> aux (h::acc) t
        |
        Many l :: t -> aux (aux acc l) t 
in aux [] (List.rev str)

(*Compress a 'a list*)
let rec compress = function
    | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
    | smaller -> smaller;;

