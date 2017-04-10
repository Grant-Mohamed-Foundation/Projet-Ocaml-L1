(* point.mli *)

              (*********************************)
              (*                               *)
              (* Specification du module Point *)
              (*                               *)
              (*********************************)

(***************************************************************)

type point = { x : int ; y : int }

type nuage = point list

type polygone = point list

(* un point est un enregistrement de coordonnees ; un nuage est
   un ensemble de points ranges dans une liste ; un polygone est
   represente par la liste (ordonnee) de ses sommets consécutifs *)

(***************************************************************)

val gen_rectangle   : int -> nuage
val gen_cercle      : int -> nuage
val gen_papillon    : int -> nuage
val gen_cerf_volant : int -> nuage
val gen_soleil      : int -> nuage
val gen_poisson     : int -> nuage 

(* genere aleatoirement des nuages de points ; le parametre
   des fonctions precedente est le nombre de points du nuage ;
   chacune d'elles produit un nuage d'une forme differente ;
   les nuages generes peuvent comporter des doublons *)

(***************************************************************)

(* Fonctions graphiques : *)

val init     : unit -> unit
val vider    : unit -> unit
val terminer : unit -> unit

val tracer_point : point -> unit
(* dessine un point sur la fenetre graphique *)

val tracer_nuage : nuage -> unit
(* dessine tous les points d'un nuage sur la fenetre graphique *)

val tracer_polygone : polygone -> unit
(* trace un polygone sur la fenetre graphique *)

(***************************************************************)

