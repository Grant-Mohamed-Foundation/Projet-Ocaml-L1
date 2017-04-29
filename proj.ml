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

         
