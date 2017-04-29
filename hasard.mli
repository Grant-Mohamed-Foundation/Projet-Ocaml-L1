(* hasard.mli *)

      (**********************************)
      (*                                *)
      (* Specification du module Hasard *)
      (*                                *)
      (**********************************)

(*************************************************)

val init_random : unit -> unit

(* (init_random ()) permet d'initialiser
   le generateur aleatoire. On ne l'utilise
   qu'une fois en debut d'une session Ocaml. *)

(*************************************************)

val random_list : int -> int -> int list

(* (random_list b n) retourne une liste
   de n entiers compris entre 0 et (b - 1). *)

(*************************************************)
