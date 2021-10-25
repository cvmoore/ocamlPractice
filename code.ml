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