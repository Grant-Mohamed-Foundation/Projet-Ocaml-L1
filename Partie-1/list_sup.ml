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

let rec partitionne_bis liste liste1 liste2 =
    match liste with
    [] -> liste1 , liste2
    |[x] -> (x::liste1) , liste2
    |x::y::r -> partitionne_bis r (x::liste1) (y::liste2);; (* ici probleme car les listes sont inverser, donc [1;2;3;4;5;6] donne [5;3;1],[6;4;2] *)

let partitionne liste = partitionne_bis liste [] [] ;;


(********************** fusionne ***********************)

(* elle doit prendre 2 listes et les fusionner de type [1;3;5] et [2;4;6] ca donne [1;2;3;4;5;6] *)

let rec fusionne inf liste1 liste2 =
    match (liste1,liste2) with
    ([],[]) -> []
    |([],_) -> liste2
    |(_,[]) -> liste1
    |(x::r1,y::r2) -> if inf x y
                        then x::y::(fusionne inf r1 r2)
                        else y::x::(fusionne inf r1 r2);;

(********************* tri_partition_fusion *******************)

let tri_partition_fusion inf l =
    let (liste1, liste2) = partitionne l in
        fusionne inf (tri_selection_min (<=) liste1) (tri_selection_min (<=) liste2) ;;
   

(********************** fonction de tri finale **********************)

(* faire des tests et trouver la fonction de tri la plus efficace *)