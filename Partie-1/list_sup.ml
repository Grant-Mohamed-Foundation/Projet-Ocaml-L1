(* list_sup.ml *)

        (***********************************)
        (*                                 *)
        (* Implantation du module list_sup *)
        (*                                 *)
        (***********************************)
        
        

(************* Tri par selection du minimum ******************)


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

let rec partitionne_bis liste listePaire listeImpaire =
    match liste with
    [] -> listePaire , listeImpaire
    |[x] -> (x::listePaire) , listeImpaire
    |x::y::r -> partitionne_bis r (x::listePaire) (y::listeImpaire);;

let partitionne liste = partitionne_bis liste [] [] ;;


(********************** fusionne ***********************)

let fusionne inf liste1 liste2 = if inf liste1 liste2
                                 then liste1 @ liste2
                                 else liste2 @ liste1;;


(********************* tri_partition_fusion *******************)

let tri_partition_fusion inf l =
    let (liste1, liste2) = partitionne l in
        fusionne inf (tri_selection_min (<=) liste1) (tri_selection_min (<=) liste2) ;;
   

(********************** fonction de tri finale **********************)

