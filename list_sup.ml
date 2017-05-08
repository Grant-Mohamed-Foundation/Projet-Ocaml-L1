(* list_sup.ml *)

        (***********************************)
        (*                                 *)
        (* Implantation du module list_sup *)
        (*                                 *)
        (***********************************)
        
        
        

(**************************  selectionne  ******************************)

let rec selectionne inf l = 
    match l with
    [] -> failwith "La liste est vide"
    | x::[] -> x
    | x::y::[] -> if inf x y
                  then x
                  else y
    | x::(y::r) -> if inf x y
                   then selectionne inf (x::r)
                   else selectionne inf (y::r) ;;
    
(************************** supprime ******************************)

let rec supprime x l =
    match l with
    [] -> l
    |a::[] -> if a == x
              then []
              else [a]
    |a::r -> if a == x
             then r
             else a::(supprime x r);;
             
(************************** tri_selection_min  ******************************)

let rec tri_selection_min inf liste =
    match liste with
    [] -> []
    |[x] -> [x]
    |x::(y::[]) -> if inf x y
                 then x::y::[]
                 else y::x::[]
    |x::r -> (selectionne inf liste)::(tri_selection_min inf (supprime (selectionne inf liste) liste));;


(********************** partitionne **********************)

let rec partitionne l =
    match l with 
    [] -> ([], [])
    |x::[] -> (l, [])
    |x::y::r -> let (l1, l2) = partitionne r in
        (x::l1, y::l2);;


(********************** fusionne ***********************)

let rec fusionne inf liste1 liste2 =
    match (liste1,liste2) with
    |([],_) -> liste2
    |(_,[]) -> liste1
    |(x::r1,y::r2) -> if inf x y
                      then x::(fusionne inf r1 liste2)
                      else y::(fusionne inf liste1 r2);;

(********************* tri_partition_fusion *******************)

let rec tri_partition_fusion_bis inf (l1, l2) =
    match (l1, l2) with
    ([], []) -> failwith "Erreur, listes vides"
    |(x::r, []) -> [x]
    |([], y::r2) -> [y]
    |(x::r, y::r2) -> fusionne inf (tri_partition_fusion_bis inf (partitionne l1)) (tri_partition_fusion_bis inf (partitionne l2))

let tri_partition_fusion inf l =
    tri_partition_fusion_bis inf (partitionne l);;
   

(********************** fonction de tri finale **********************)

(* En moyenne 0.114 seconde pour trier un liste de 100 éléments contenant des entiers compris entre 0 et 1000 avec tri_selection_min *)

(* En moyenne 0.062 seconde pour trier un liste de 100 éléments contenant des entiers compris entre 0 et 1000 avec tri_partition_fusion, soit 2 fois moins que le tri_selection_min *)

let tri = tri_partition_fusion ;;


(*********************** min_list *************************)

let min_list inf l =
    selectionne inf l;;
    
(********************* suppr_doublons **********************)

let rec suppr_doublons liste =
    match liste with
    [] -> []
    |[x] -> [x]
    |x::y::r -> if x = y
                then suppr_doublons (y::r)
                else x::(suppr_doublons (y::r)) ;;

