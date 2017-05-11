(* proj.ml *)

          (*******************************)
          (*                             *)
          (* Implantation du module Proj *)
          (*                             *)
          (*******************************)


#load "list_sup.cmo" ;;
#load "point.cmo" ;;
#load "pile.cmo" ;;

open List_sup ;;
open Point ;;
open Pile ;;


(* infc *)

let infc point1 point2 =
    if point1.y < point2.y
    then true
    else point1.y = point2.y && point1.x <= point2.x ;;
         
         
(* det *)
let det q0 q1 q2 =
    (q1.x - q0.x)*(q2.y - q0.y) - (q2.x - q0.x) * (q1.y - q0.y);;
        
        
(* sca *)

let sca q0 q1 q2 =
    (q1.x - q0.x)*(q2.x - q0.x) + (q1.y - q0.y)*(q2.y - q0.y);;

        
(* infg *)

let infg w p1 p2 =
    if p1 = w || p1 = p2
    then true
    else if p1 != p2 && p2 != w && p1 != w
         then (det w p1 p2) > 0
         else (det w p1 p2) = 0 && (sca p1 w p2) < 0 ;;
                   

(* test de tri_points, ca marche avec le test juste au dessus apres faut voir avec d'autre xD *)

let tri_points listePoint =
     suppr_doublons (tri (infg (min_list infc listePoint)) listePoint);;

     
(* algo_graham *)
(* Je pense que ça fonctionne presque mais je trouve pas l'erreur xD *)

let rec algo_graham liste pile =
    let s = subtop pile
    and p = top pile in
        match liste with
        [] -> pile
        |x::r -> if det p x s > 0
                 then algo_graham r (empiler x pile)
                 else if det p x s < 0
                      then algo_graham liste (depiler pile)
                      else algo_graham r (empiler x (depiler pile)) ;;
                       
                       
(* env_graham *)

let env_graham listePoint =
    let liste = tri_points listePoint in
        match liste with
        [] -> failwith "Erreur, liste vide"
        |[x] -> failwith "Pas assez d'element dans la liste"
        |x::y::r -> list_of_pile(algo_graham liste (empiler y (empiler x vide))) ;;

(* env *)

(*let env g n =
    let l = (g n) in
    (
        vider ()
        tracer_nuage l
        tracer_polygone (env_graham l)
    ) ;;
*)
