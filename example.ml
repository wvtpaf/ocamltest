let factors n = 
    let rec aux d n =
      if n = 1 then [] else
        if n mod d = 0 then d :: aux d (n / d) else aux (d+1) n
    in
    let rec find e = function
        | [] -> false
        | h::t -> h = e || find e t
    in
    let rec cut_redundant l1 l2 =
        match l1 with
        | [] -> l2
        | h::t -> if find h l2 then cut_redundant t l2
                  else cut_redundant t (h::l2)
    in
    List.rev (cut_redundant (aux 2 n) [])
