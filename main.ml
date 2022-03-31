(**********************************************

       Projet Programmation Fonctionnelle

                GHITA Alessandro
                GELINAUD Clement
                BENMOUSSATI Souhail

**********************************************)

(* QUESTION 1 : *)

(* Définition des types de base de miniml *)

type t_typePrimitive =
	| Entier
  | Bool
  | Char
;;

type t_type =
	| T of t_typePrimitive
  | Fonction of t_type * t_type
  | Tuple of t_type * t_type
;;

type primitive = 
  | Un
  | Deux
  | Trois
  | Quatres
  | Cinq
  | A
  | B
  | C
  | D
  | Vrai
  | Faux
  | Plus
  | Moins
  | Inferieur
  | Superieur
  | Condition
;;

(* Défini le nom d'une variable ou d'une fonction *)
type name = string;;


type t_expr =
  | Var of name
  | Constante of primitive
  | Fonction of name * t_type * t_expr
  | Application of t_expr * t_expr
  | Tuple of t_expr * t_expr
  | Let of name * t_type * t_expr * t_expr
;;


(* QUESTION 2 : *)

let primitive_print primitive =
	match primitive with
	| Un -> "Un"
  | Deux -> "Deux"
  | Trois -> "Trois"
  | Quatres -> "Quatres"
  | Cinq -> "Cinq"
  | A -> "A"
  | B -> "B"
  | C -> "C"
  | D -> "D"
  | Vrai -> "Vrai"
  | Faux -> "Faux"
  | Plus -> "Plus"
  | Moins -> "Moins"
  | Inferieur -> "Inferieur"
  | Superieur -> "Superieur"
  | Condition -> "Condition"
;;
	
















(* CE QUE J'AVAIS FAIT *)


(* QUESTION 1 : *)
(* Définir les types OCaml, pour les types et les expressins du langage mini-ML*)

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

type t_type =
	| Int
	| Tableau of t_type * t_type
	| Produit of t_type * t_type
;;

type expr =
	| Var of string
	| Const of int
	| Op of string
	| Fun of string * t_type * expr
	| App of expr * expr
	| Paire of expr * expr
	| Let of string * expr * expr
;;

(* On réalise l'environnement avec une structure persistante en utilisant le module map *)

module Smap = Map.Make(String);;
type env = t_type Smap.t;;

(* Concernant la performence : on a un arbre équilibré. *)
(* La recherche et l'insertion se font en O(log(n)) *)

(* Question 2 :
Définir une fonction "d’affichage" des expressions mini-ML qui prend une expression et retourne
l’expression sous la forme d’une chaîne de caractères, écrite de manière habituelle.

Définir quelques exemples d’expressions mini-ML et vérifier leur forme avec la fonction précédente. *)

let rec t_expr env = function
	| Const _ -> Int
	| Var x -> Smap.find x env
	| Op "+" -> Tableau (Produit (Int, Int), Int)
	| Paire (e1, e2) -> Produit (t_expr env e1, t_expr env e2)
	| Fun (x, ty, e) -> Tableau (ty, t_expr (Smap.add x ty env) e) (* Pour les fonctions, le type de la variable est donnée *)
	| Let (x, e1, e2) -> t_expr (Smap.add x (t_expr env e1) env) e2 (* Intérêt de l apersistence de env *)
	| App (e1, e2) -> begin match t_expr env e1 with
    | Tableau (ty2, ty) ->
        if t_expr env e2 = ty2
        then ty
        else failwith("erreur : argument de mauvais type")
    | _ ->
        failwith("erreur : fonction attendue")
		end
;;

t_expr
   (Let ("f",
     Fun ("x", Tint, App (Op "+", Pair (Var "x", Const 1))),
     App (Var "f", Const 2)));;







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