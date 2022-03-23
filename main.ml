(**********************************************

       Projet Programmation Fonctionnelle

                GHITA Alessandro
                GELINAUD Clement
                BENMOUSSATI Souhail

**********************************************)

(* QUESTION 1 : *)
(* Définir les types OCaml, pour les types et les expressins du langage mini-ML*)

type t_type =
	| Nombre of int
	| Boolean of bool
	| Charactere of char
	| String of string
;;

type t_expr =
	| Var of string
	| Const of int
	| Op of string
	| Fun of string * t_type * t_expr
	| App of t_expr * t_expr
	| Paire_expr of t_expr * t_expr
	| Let of string * t_expr * t_expr
;;

(*
Les constantes sont celles des types de base (int, bool, unit . . . ).
Les opérateurs op sont les symboles d’opérations primitives (+,-, etc.).

constante => 1, 2, true...
primitive => x, +, fst...
fonction => fun x -> e
application => e e
apre => (e, e)
liaison locale => let x = e in e
*)


(* Q2 : Définir une fonction "d’affichage" des expressions mini-ML qui prend une expression et retourne
        l’expression sous la forme d’une chaîne de caractères, écrite de manière habituelle.
        Définir quelques exemples d’expressions mini-ML et vérifier leur forme avec la fonction
        précédente.
*)

(* Q3 : Nous allons utiliser les listes d’associations OCaml pour représenter les environnements. Définir
        l’environnements Ep comprenant les constantes et les opérations primitives du langage mini-ML.
*)


(* Q4 : Définir une fonction de vérification de type, qui prend en argument un environnement et une
        expression mini-ML et retourne le type de l’expression si l’expression est bien typée et lève une
        erreur sinon. Cette fonction mettra en oeuvre les règles d’inférence monomorphe.
*)

(* Q5 : Utiliser la fonction de vérification de type de la question 4 pour calculer le type des expressions
        mini-ML typé suivantes :
        fun x : char -> (let succ : int -> int = fun x : int -> + (x, 1) in (succ 1, x))
        fun x : char -> (let succ : int -> int = fun y : int -> + (y, 1) in (succ 1, x))
        Ont-elles le même type ? Sinon, modifier la fonction précédente pour prendre en compte les
        variables homonymes.
*)