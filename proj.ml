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
(*
let algo_graham liste pile =
    
    let p = subtop pile
    and r = top pile in
    
        match liste with
        [] -> exception Erreur_pile_vide
        |[x] -> pile (* faut trouver ce qu'il faut mettre *)
        |x::r -> if det(s x r) > 0 (* faut trouver par quoi remplacer s et r (et je suis pas sur pour x) *)
                 then algo_graham r (empiler x)
                 else if det(s x r) < 0
                      then algo_graham liste (depiler x)
                      else algo_graham r pile ;;
                    
 *)                   
(* env_graham *)



(* env *)

(*let env g n =
    let l = (g n) in
    (
        vider () ;
        tracer_nuage l ;
        tracer_polygone (env_graham l)
    ) ;;
    
    *)
