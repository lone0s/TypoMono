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
	| Fonction of (t_type -> t_type)
	| Paire of t_type * t_type
	| Nil
;;

type t_expr =
	| Variable of string (* Identificateur (nom de variable) *)
	| Car of char
	| App of t_expr * t_expr
	| Paire_expr of t_expr * t_expr
;;

type t_expr = Var  of string                             (* Variables / Identificateur *)
            | Char of char                               (* Caractère                  *)
            | Int  of int                                (* Entier                     *)
            | Bool of bool                               (* Booléen                    *)
            | Op   of string * t_expr                    (* Opérateur                  *)
            | Func of string * t_type * t_expr           (* Fonction anonyme           *)
            | App  of t_expr * t_expr                    (* Application de fonction    *)
            | Pair of t_expr * t_expr                    (* Tuple                      *)
            | Let  of string * t_type * t_expr * t_expr  (* Declaration de variable    *)
;;


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