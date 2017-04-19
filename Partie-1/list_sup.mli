(* list_sup.mli *)

        (************************************)
        (*                                  *) 
        (* Spécification du module list_sup *)
        (*                                  *)
        (************************************)
        

(************************************************)        
        
val tri : ('a -> 'a -> bool) -> 'a list -> 'a list

(* tri (<=) liste renvoie une liste triée selon le minimum *)

(************************************************)

val min_list : ('a -> 'a -> bool) -> 'a list -> 'a

(* min_list (<=) liste renvoie le minimum de la liste *)

(************************************************)

val suppr_doublons : 'a list -> 'a list

(* suppr_doublons liste prend en parametre une liste triée et supprime les doublons éventuels. *)

(************************************************)