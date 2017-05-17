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


(****************** infc *******************)

let infc point1 point2 =
    if point1.y < point2.y
    then true
    else point1.y = point2.y && point1.x <= point2.x ;;

(* val infc : point -> point -> bool *)
    
       
(********************** det *********************)

let det q0 q1 q2 =
    (q1.x - q0.x)*(q2.y - q0.y) - (q2.x - q0.x) * (q1.y - q0.y);;
        
(* val det : point -> point -> point -> int *)
        

(****************** sca ******************)

let sca q0 q1 q2 =
    (q1.x - q0.x)*(q2.x - q0.x) + (q1.y - q0.y)*(q2.y - q0.y);;

(* val sca : point -> point -> point -> int *)
    
        
(**************** infg ****************)

let infg w p1 p2 =
    if p1 = w 
    then true
    else if p1 = p2
         then true
         else if p2 != w && (det w p1 p2) > 0
              then true
              else p2 != w && (det w p1 p2) = 0 && (sca p1 w p2) < 0 ;;
              
(* val infg : point -> point -> point -> bool *)
         

(************************ tri_points *********************)

let tri_points listePoint =
     suppr_doublons (tri (infg (min_list infc listePoint)) listePoint);;

(* val tri_points : point list -> point list *)
     
     
(********************* algo_graham *********************)

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
                

(**************************** env_graham ***************************)

let env_graham listePoint =
    match tri_points listePoint with
    [] -> failwith "Erreur, liste vide"
    |[x] -> failwith "Pas assez d'element dans la liste"
    |x::y::r -> list_of_pile(algo_graham r (empiler y (empiler x vide))) ;;

(* val env_graham : point list -> point list *)
        

(************************** env ************************)

let env g n =
    let l = (g n) in
        vider () ;
        tracer_nuage l ;
        tracer_polygone (env_graham l) ;;

(* val env : ('a -> nuage) -> 'a -> unit *)


(*************************** infj *************************)

let infj s p1 p2 =
    if p2 = s
    then true
    else if p2 = p1
         then true
         else if det s p1 p2 > 0
              then true
              else det s p1 p2 = 0 && sca p1 s p2 > 0 ;;
              
(* val infj : point -> point -> point -> bool *)


(********************** algo_jarvis ***********************)

let rec algo_jarvis nuage p0 pile =
    let p1 = min_list (infj(top pile)) nuage in
        if p1 = p0 (* cas d'arret si on a fais le tour de l'enveloppe *)
        then pile
        else algo_jarvis nuage p0 (empiler p1 pile) ;; (* on ajoute le plus petit point different de p0 (du point precedent) *)

(* val algo_jarvis : nuage -> point -> pile -> pile *)


(*************** env_jarvis ***************)

let env_jarvis nuage =
    let p0 = min_list infc nuage in (* on defini p0 etant le minimum du nuage, selon infc *)
        list_of_pile(algo_jarvis nuage p0 (empiler p0 vide)) ;; (* on lance l'algo_jarvis avec p0 et une pile contenant uniquement p0 *)
        
(* val point list -> point list *)


(********************* envj **********************)

let envj g n =
    let l = (g n) in
        vider () ;
        tracer_nuage l ;
        tracer_polygone (env_jarvis l) ;;
        
(* val envj : ('a -> nuage) -> 'a -> unit *)