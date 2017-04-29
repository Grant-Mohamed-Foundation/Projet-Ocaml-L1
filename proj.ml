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
