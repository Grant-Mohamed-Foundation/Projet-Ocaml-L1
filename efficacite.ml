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

let temps_exe_min f a =
    let f a = f (<) a in
        let debut_temps = Sys.time () in
            ignore (f a);
                let fin_temps = Sys.time () in
                    fin_temps -. debut_temps ;;
