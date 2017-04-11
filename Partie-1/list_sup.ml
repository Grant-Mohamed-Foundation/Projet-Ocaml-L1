(* list_sup.ml *)

      (***********************************)
      (*                                 *)
      (* Implantation du module list_sup *)
      (*                                 *)
      (***********************************)



(********************** selectionne **********************)

let rec selectionne inf l = 
	match l with
	[] -> failwith "Erreur liste vide"
	|[x] -> x
	|x::y::[] -> if inf x y
				 then x
				 else y
				 
	| x::y::r -> if inf x y
				 then selectionne inf (x::r)
				 else selectionne inf (y::r);;



(************************  supprime  ******************************)

let rec supprime x l =
    match l with
    [] -> []
    |a::[] -> if a == x
              then []
              else [a]
    |a::y::[] -> if a == x
                 then [y]
                 else supprime x [y]
    |a::(y::r) -> if a == x
                  then y::r
                  else if y == x
                       then a::r
                       else a::(y::(supprime x r)) ;;



(********************** tri_selection_min **********************)

let rec tri_selection_min inf liste =
	match liste with
	[] -> liste
	|x::r -> selectionne inf liste :: tri_selection_min inf (supprime(selectionne inf l))
	;;
