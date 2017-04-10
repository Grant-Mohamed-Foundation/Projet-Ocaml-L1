(* Exercice 2.1 *)

let concatPhrase p1 p2 = p1 ^ p2;;

let longueurPhrase p = String.length p;;

(* 1 *)
let rec is_occurence_bis x p i =
if i == -1
then false
else if x == p.[i]
	 then true
	 else is_occurence_bis x p (i-1);;
	 
let is_occurence x p = is_occurence_bis x p (String.length p-1);;


(* 2 *)
let rec nb_occurence_bis x p i compteur=
if i == -1
then compteur
else if x == p.[i]
	 then nb_occurence_bis x p (i-1) (compteur + 1)
	 else nb_occurence_bis x p (i-1) compteur;;
	 
let nb_occurence x p = nb_occurence_bis x p (String.length p -1) 0;;


(* 3 *)
let rec is_palindrome_bis s n i =
if i == -1
then true
else if s.[n] == s.[i]
	 then is_palindrome_bis s (n+1) (i-1)
	 else false;;

let is_palindrome s = is_palindrome_bis s 0 (String.length s -1);;


(* 4 *)

(* prefix *)
let rec is_prefix_bis p1 p2 i=
if i == String.length p1
then true
else if p1.[i] == p2.[i]
	 then is_prefix_bis p1 p2 (i+1)
	 else false;;

let is_prefix p1 p2 = is_prefix_bis p1 p2 0;;

(* suffix *)
    let rec is_suffix_bis p1 p2 i=
    if i == (String.length p2) - (String.length p1)
    then true
    else if p1.[i] == p2.[i+(String.length p2)]
        then is_prefix_bis p1 p2 (i-1)
        else false;;

let is_suffix p1 p2 = is_suffix_bis p1 p2 (String.length p2);;

(* factor *)
let rec is_factor_bis p1 p2 i j =
if i == (String.length p1)
then true
else if j == (String.length p2) && i != (String.length p1)-1
     then false
     else if i == 0
          then if p1.[i] == p2.[j]
               then is_factor_bis p1 p2 (i+1) (j+1)
               else is_factor_bis p1 p2 i (j+1)
          else if p1.[i] == p2.[j]
			   then is_factor_bis p1 p2 (i+1) (j+1)
			   else is_factor_bis p1 p2 0 (j+1) ;;

let is_factor p1 p2 = is_factor_bis p1 p2 0 0;;
