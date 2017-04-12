(* pile.mli *)

             (********************************)
             (*                              *)
             (* Specification du module Pile *)
             (*                              *)
             (********************************)

type 'a pile

(* declaration d'une pile :
   aucune indication sur l'implantion n'est donnee *)

(**********************************************************)

val vide : 'a pile

(* une constante du type pile *)

val empiler : 'a -> 'a pile -> 'a pile

(* ajoute un element au sommet de la pile *)

(**********************************************************)

exception Erreur_pile_vide

val depiler : 'a pile -> 'a pile

(* retourne la pile sans son sommet ; leve l'exception
   Erreur_pile_vide si on tente de depiler une pile qui
   est vide *)

(**********************************************************)

val top    : 'a pile -> 'a
val subtop : 'a pile -> 'a

(* retourne les sommet et sous-sommet d'une pile sans toucher
   a la pile elle-meme ; leve l'exception Erreur_pile_vide si
   on tente d'acceder au sommet d'une pile vide ou au
   sous-sommet d'une pile n'ayant qu'un element *)

(**********************************************************)

val list_of_pile : 'a pile -> 'a list

(* retourne la liste de tous les elements contenus dans une
   pile, de la base au sommet (ie, le dernier element de la
   liste resultat est le sommet de la pile passe en argument *)

(**********************************************************)

