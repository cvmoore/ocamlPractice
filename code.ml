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
