(* proj.ml *)


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
    else if point1.y = point2.y && point1.x <= point2.x
         then true
         else false ;;
         
         
(* det *)

let det q0 q1 q2 =
    let q0q1 = { x = q1.x - q0.x ; y = q1.y - q0.y }
    and q0q2 = { x = q2.x - q0.x ; y = q2.y - q0.y } in
        (q0q1.x * q0q2.y) - (q0q2.x * q0q1.y) ;;
        
        
(* sca *)

let sca q0 q1 q2 =
    let q0q1 = { x = q1.x - q0.x ; y = q1.y - q0.y }
    and q0q2 = { x = q2.x - q0.x ; y = q2.y - q0.y } in
        (q0q1.x * q0q2.x) + (q0q2.y * q0q1.y) ;;

        
(* infg *)

let infg w p1 p2 =
    if p1 = w || p1 = p2
    then true
    else if p1 != p2 && p2 != w && p1 != p2
         then (det w p1 p2) > 0
         else (det w p1 p2) = 0 && (sca p1 w p2) < 0 ;;
                   
                   
(* tri_points -> ça fonctionne pas... il faut que je reprenne depuis le début parce qu'il faut utiliser les fonctions de list_sup... *)

let rec min_point l = match l with
                  [] -> failwith "liste vide"
                  |x::[] -> x
                  |x::y::[] -> if infc x y
                               then x
                               else y
                  |x::y::r -> if infc x y
                             then min_point (x::r)
                             else min_point (y::r);;

let rec tri_points l = let p0 = min_point l in 
                        match l with
                        [] -> l
                        |x::[] -> [x]
                        |x::y::[] -> if infg p0 x y
                                     then x::y::[]
                                     else y::x::[]
                        |x::y::r -> if not (infg p0 x y)
                                    then tri_points (y::x::r)
                                    else x::(tri_points (y::r)) ;;

(* test: tri_points [{x=5;y=5};{x=3;y=3};{x=1;y=1};{x=2;y=2};{x=4;y=4}] ;; *)


(* test de tri_points, ca marche avec le test juste au dessus apres faut voir avec d'autre xD *)

let tri_points listePoint =
    let p0 = min_list infc listePoint in
        (* On définit infg2 de maniere a avoir uniquement 2 paramètre pour utiliser la fonction tri : *)
        let infg2 x y = infg p0 x y in
            suppr_doublons(tri infg2 listePoint);;

            
(* algo_graham *)

let algo_graham liste pile =
    
    let p = subtop pile
    and r = top pile in
    
        match liste with
        [] -> exception Erreur_pile_vide
        |[x] -> pile (* faut trouver ce qu'il faut mettre *)
        |x::r -> if det(s x r) > 0 (* faut trouver par quoi remplacer s et r (et je suis pas sur pour x) *)
                then algo_graham r (empiler x)
                else if det(s x r) < 0
                    then algo_graham r (depiler x)
                    else algo_graham r pile ;;
                    
                    
