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


(********************** reverse *************************)

(* fonction permettant de résoudre le problème lié a la fonction partitionne (qui renvoyais les listes dans le mauvais ordres) *)
let rec reverse_bis liste =
    match liste with
    [] -> []
    |x::r -> (reverse_bis r) @ [x];;

let reverse (liste1,liste2) = reverse_bis(liste1) , reverse_bis(liste2) ;;


(********************** partitionne **********************)

let rec partitionne_bis liste liste1 liste2 =
    match liste with
    [] -> liste1 , liste2
    |[x] -> (x::liste1) , liste2
    |x::y::r -> partitionne_bis r (x::liste1) (y::liste2);;

let partitionne liste = reverse(partitionne_bis liste [] []) ;;


(********************** fusionne ***********************)

let rec fusionne inf liste1 liste2 =
    match (liste1,liste2) with
    |([],_) -> liste2
    |(_,[]) -> liste1
    |(x::r1,y::r2) -> if inf x y
                      then x::(fusionne inf r1 liste2)
                      else y::(fusionne inf liste1 r2);;

(********************* tri_partition_fusion *******************)

let tri_partition_fusion inf l =
    let (liste1, liste2) = partitionne l in
        fusionne inf (tri_selection_min (<=) liste1) (tri_selection_min (<=) liste2) ;;
    
    
(********************* tri_selection_max *******************)
        
let rec tri_selection_max_bis inf l1 l2=
    match l1 with
    [] -> []
    |[x] -> [x]
    |x::(y::[]) -> if inf x y
                 then x::l2
                 else y::l2
    |x::r -> (selectionne inf l1)::(tri_selection_max_bis inf (supprime (selectionne inf l1) l1) l2);;
    

let tri_selection_max inf liste = tri_selection_max_bis inf liste [] ;;
   
   
(********************* tri_insertion *******************)

let rec insere inf x l =
    match l with
    [] -> [x]
    |y::r -> if inf x y
             then x::l
             else y::(insere inf x r) ;;

let rec enumere inf at dt =
    match at with
    [] -> dt
    |x::r -> enumere inf r (insere inf x dt);;
    

let tri_insertion inf l = enumere inf l [] ;;

(********************* tri_pivot*******************)
   
let rec separe_inf_eq_sup_bis comp x l i m f= 
    match l with
    [] -> (i,m,f)
    |y::r -> if y = x
             then separe_inf_eq_sup_bis comp x r i (y::m) f
             else if comp y x
                  then separe_inf_eq_sup_bis comp x r (y::i) m f
                  else separe_inf_eq_sup_bis comp x r i m (y::f) ;;
                  
let separe_inf_eq_sup comp x l = separe_inf_eq_sup_bis comp x l [] [] [] ;;
                  
let rec concat_l (i,m,f) = i::m::f::[] ;;

let rec tri_pivot infeq l =
    match l with
    [] -> l
    |x::r -> tri_pivot infeq (concat_l (separe_inf_eq_sup infeq x r)) ;; 
   

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

