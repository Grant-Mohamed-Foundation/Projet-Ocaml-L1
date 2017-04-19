(* efficacite.ml *)

        (*************************************)
        (*                                   *)
        (* Implantation du module efficacite *)
        (*                                   *)
        (*************************************)

        
open Sys ;;

(*************************************************)

(* calcul le temps que mets un programme à s'exécuter, avec f le nom du programme et x son argument *)

let time f x =
    let t = Sys.time() in
        let fx = f x in
            Printf.printf "Execution time: %fs\n" (Sys.time() -. t);
                fx ;;

(*************************************************)

(* comme dans nos fonctions on a deux arguments j'ai fait ce bis pour revenir à un argument car quoi qu'on compare les programmes devraient réagir de la même manière non?? *)

let temps_exe_min f a =
    let f a = f (<) a in
        let debut_temps = Sys.time () in
            ignore (f a);
                let fin_temps = Sys.time () in
                    fin_temps -. debut_temps ;;
                
(*************************************************)                

(* et là j'ai pas encore trouver le moyen de remplacer f1 et f2 en commentaire par les noms complets, du coup, obligé de définir les noms complets des fonctions... ça c'est plutot dégueux par contre mais bon je suis sur qu'il ya une solution! *)

let test_efficacite f1 f2 b n =
    let l = (random_list b n) in
    if temps_exe_min f1 l < temps_exe_min f2 l
    then "le plus rapide est tri_selection_min: ", temps_exe_min f1 l
    else if temps_exe_min f1 l > temps_exe_min f2 l
         then "le plus rapide est  tri_partition_fusion: ", temps_exe_min f2 l
         else failwith "tri_selection_min et tri_partition_fusion ont la meme efficacite" ;;
         
(* 
et après tu mets ça (en-dessous) avec b et n les nombres que tu veux mais pas trop grands sinon ça mets 10min à te donner le résultat... et aussi ya un petit soucis mais je t'expliquerais quand on s'appelera parce que c'est trop long à expliquer xD 

test_efficacite tri_selection_min tri_partition_fusion b n ;;
*)
