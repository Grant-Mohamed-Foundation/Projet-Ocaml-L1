(* hasard.ml *)

        (*********************************)
        (*                               *)
        (* Implantation du module Hasard *)
        (*                               *)
        (*********************************)

open Sys ;;

(*************************************************)

let init_random () =
  let s = "/tmp/la_date_de_" ^ (getenv "LOGNAME") in
  let _ = command ("date +\"%M%H%j\" > " ^ s) in
  let c = open_in s in
  let n = int_of_string (input_line c) in
  ( close_in c ; remove s ; Random.init n ) ;;

(*************************************************)

let rec random_list b n =
  if n <= 0
  then []
  else (Random.int b)::(random_list b (n - 1)) ;;

(*************************************************)
