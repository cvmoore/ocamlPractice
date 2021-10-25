(*Returns the last element of a list *)
let rec last list = 
    match list with
    [h] -> Some h
    |
    h::t -> last t
    |
    [] -> None
