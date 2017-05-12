(* proj.ml *)

          (*******************************)
          (*                             *)
          (* Implantation du module Proj *)
          (*                             *)
          (*******************************)


(* chargement des modules *)
#load "list_sup.cmo" ;;
#load "point.cmo" ;;
#load "pile.cmo" ;;

(* ouverture des modules *)
open List_sup ;;
open Point ;;
open Pile ;;


(* infc *)

let infc point1 point2 =
    if point1.y < point2.y
    then true
    else point1.y = point2.y && point1.x <= point2.x ;;

(* val infc : point -> point -> bool *)
    
         
(* det *)
let det q0 q1 q2 =
    (q1.x - q0.x)*(q2.y - q0.y) - (q2.x - q0.x) * (q1.y - q0.y);;
        
(* val det : point -> point -> point -> int *)
        
        
(* sca *)

let sca q0 q1 q2 =
    (q1.x - q0.x)*(q2.x - q0.x) + (q1.y - q0.y)*(q2.y - q0.y);;

(* val sca : point -> point -> point -> int *)
    
        
(* infg *)

let infg w p1 p2 =
    if p1 = w || p1 = p2
    then true
    else if p1 != p2 && p2 != w && p1 != w
         then (det w p1 p2) > 0
         else (det w p1 p2) = 0 && (sca p1 w p2) < 0 ;;
         
(* val infg : point -> point -> point -> bool *)
         

(* tri_points *)

let tri_points listePoint =
     suppr_doublons (tri (infg (min_list infc listePoint)) listePoint);;

(* val tri_points : point list -> point list *)
     
     
(* algo_graham *)

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

(* val algo_graham : point list -> point pile -> point pile *)
                      
                       
(* env_graham *)

let env_graham listePoint =
    let liste = tri_points listePoint in
        match liste with
        [] -> failwith "Erreur, liste vide"
        |[x] -> failwith "Pas assez d'element dans la liste"
        |x::y::r -> list_of_pile(algo_graham r (empiler y (empiler x vide))) ;;

(* val env_graham : point list -> point list *)
        
        
(* env *)

let env g n =
    let l = (g n) in
    (
        vider () ;
        tracer_nuage l ;
        tracer_polygone (env_graham l)
    ) ;;

(* val env : ('a -> nuage) -> 'a -> unit *)
