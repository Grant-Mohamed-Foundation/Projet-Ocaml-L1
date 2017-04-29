(* pile.ml *)

          (*******************************)
          (*                             *)
          (* Implantation du module Pile *)
          (*                             *)
          (*******************************)


type 'a pile = 'a list ;;

(******************************************************)

let vide = [] ;;

let empiler x p = x::p ;;

exception Erreur_pile_vide ;;

let depiler p =
  match p with
    [] -> raise Erreur_pile_vide
  | x::r -> r ;;

let top p =
  match p with
      [] -> raise Erreur_pile_vide
  | x::_ -> x ;;

let subtop p =
  match p with
    _::(y::_) -> y
  | _ -> raise Erreur_pile_vide ;;

let list_of_pile = List.rev ;;

(******************************************************)

