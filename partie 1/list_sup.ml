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


(********************** tri_selection_min **********************)

let rec tri_selection_min inf liste =
	match liste with
	[] -> []
	|[x] -> [x]
	|x::y::[] -> if inf x y
				 then x::y::[]
				 else y::x::[]
				 
	|x::y::r -> if inf x y
				then x::selectionne inf (y::r) :: (tri_selection_min inf r)
				else y::selectionne inf (x::r) :: tri_selection_min inf r;;
