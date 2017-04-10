(* 2.1  1) *)

let concat_phrase p1 p2 = p1^p2 ;;

let longueur_phrase p = String.length p ;;

(* 2.1  2) *)

let rec is_occurrence_bis x p i= 
	if (i = -1)
	then false
	else if (p.[i] == x)
	     then true
	     else is_occurrence_bis x p (i-1) ;;

let is_occurrence x p = is_occurrence_bis x p (String.length p-1) ;;


let rec nb_occurrence_bis x p i n =
	if (i = -1)
	then n
	else if (p.[i] == x)
	     then nb_occurrence_bis x p (i-1) (n+1)
	     else nb_occurrence_bis x p (i-1) n;;

let nb_occurrence x p = nb_occurrence_bis x p (String.length p-1) 0;;

(* 3 *)

let rec is_palindrome_bis mot avance recule = 
	if recule = -1
	then true
	else if (mot.[avance] == mot.[recule])
	     then is_palindrome_bis mot (avance+1) (recule-1)
	     else false ;;

let is_palindrome mot = is_palindrome_bis mot 0 (String.length mot-1) ;;

(* 4 *)

(* prefix *)

let rec is_prefix_bis p1 p2 compteur =
	if compteur = (String.length p1)
	then true
	else if (p1.[compteur] == p2.[compteur])
	     then is_prefix_bis p1 p2 (compteur+1)
	     else false ;;

let is_prefix p1 p2 = is_prefix_bis p1 p2 0 ;;

(* suffix *)

let rec is_suffix_bis p1 p2 compteur_p1 compteur_p2 =
	if compteur_p1 = -1
	then true
	else if (p1.[compteur_p1] == p2.[compteur_p2])
	     then is_suffix_bis p1 p2 (compteur_p1 -1) (compteur_p2 -1)
	     else false ;;

let is_suffix p1 p2 = is_suffix_bis p1 p2 (String.length p1 -1) (String.length p2 -1);;

(* factor *)

let rec is_factor_bis p1 p2 compteur_p1 compteur_p2 =
if compteur_p1 == (String.length p1)
then true
else if compteur_p2 == (String.length p2) && compteur_p1 != (String.length p1) -1
     then false
     else if compteur_p1 == 0
          then if p1.[compteur_p1] == p2.[compteur_p2]
               then is_factor_bis p1 p2 (compteur_p1 +1) (compteur_p2 +1)
               else is_factor_bis p1 p2 compteur_p1 (compteur_p2 +1)
          else if p1.[compteur_p1] == p2.[compteur_p2]
			   then is_factor_bis p1 p2 (compteur_p1 +1) (compteur_p2 +1)
			   else is_factor_bis p1 p2 0 (compteur_p2 +1) ;;

let is_factor p1 p2 = is_factor_bis p1 p2 0 0;;

(* 5 *)
(* sous phrase *)

let rec is_sous_phrase_bis p1 p2 compteur_p1 compteur_p2 =
	if compteur_p1 == -1
	then true
	else if compteur_p2 == -1
	     then false
	     else if p1.[compteur_p1] == p2.[compteur_p2]
		  then is_sous_phrase_bis p1 p2 (compteur_p1 -1) (compteur_p2 -1)
		  else is_sous_phrase_bis p1 p2 (compteur_p1) (compteur_p2 -1);;

let is_sous_phrase p1 p2 = is_sous_phrase_bis p1 p2 (String.length p1 -1) (String.length p2 -1) ;;

(* 2.2 *)
(* concat_phrase *)

let rec concat_phrase_bis p1 p2 compt_1 compt_2 = 
	if p1.[compt_1] = ' '
	then 
	else if p2.[compt_2] = ' '
	     then concat_phrase_bis p1 p2 compt_1 (compt_2 -1)
	     else (p1.[compt_1])^(p2.[compt2]) ;;

let concat_phrase p1 p2 = concat_phrase_bis p1 p2 (String.length p1 -1) (String.length p2 -1) ;;
