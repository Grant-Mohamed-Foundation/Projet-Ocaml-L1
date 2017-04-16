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


(* fonction permettant de résoudre le problème lié a la fonction partitionne (qui renvoyais les listes dans le mauvais ordres) *)
let rec reverse_bis liste =
    match liste with
    [] -> []
    |x::r -> (reverse_bis r) @ [x];;

let reverse (liste1,liste2) = reverse_bis(liste1) , reverse_bis(liste2) ;;

let rec partitionne_bis liste liste1 liste2 =
    match liste with
    [] -> liste1 , liste2
    |[x] -> (x::liste1) , liste2
    |x::y::r -> partitionne_bis r (x::liste1) (y::liste2);;

let partitionne liste = reverse(partitionne_bis liste [] []) ;;


(********************** fusionne ***********************)

let rec fusionne inf liste1 liste2 =
    match (liste1,liste2) with
    ([],[]) -> []
    |([],_) -> liste2
    |(_,[]) -> liste1
    |(x::r1,y::r2) -> if inf x y
                      then x::(fusionne inf r1 liste2)
                      else y::(fusionne inf liste1 r2);;

(********************* tri_partition_fusion *******************)

let tri_partition_fusion inf l =
    let (liste1, liste2) = partitionne l in
        fusionne inf (tri_selection_min (<=) liste1) (tri_selection_min (<=) liste2) ;;
   

(********************** fonction de tri finale **********************)

(* faire des tests et trouver la fonction de tri la plus efficace *)