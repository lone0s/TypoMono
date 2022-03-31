(**********************************************

       Projet Programmation Fonctionnelle

                GHITA Alessandro
                GELINAUD Clement
                BENMOUSSATI Souhail

**********************************************)

(* QUESTION 1 : *)

(* Définition des types de base de miniml *)

open List

type t_typePrimitive =
	| Entier
  | Bool
  | Char
;;

type t_type =
	| Type of t_typePrimitive
  | Fonction of t_type * t_type
  | Tuple of t_type * t_type
;;

type primitive = 
  | Un
  | Deux
  | Trois
  | Quatre
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

let t_typePrimitive_print monType =
	match monType with
	| Entier -> "Entier"
  | Bool -> "Bool"
  | Char -> "Char"
;;

let primitive_print primitive =
	match primitive with
	| Un -> "Un"
  | Deux -> "Deux"
  | Trois -> "Trois"
  | Quatre -> "Quatre"
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


          
let rec t_type_print monType =
  match monType with
  | Type type1 -> t_typePrimitive_print type1
  | Fonction(type1, type2) -> (
  	t_type_print type1 ^ " -> " ^ t_type_print type2
  	)
  | Tuple(type1, type2) ->
  	
  	(* L'ordre du filtrage est très important afin de ne pas rentré
  			directement dans le cas de base *)
  	match type1, type2 with
     (* Double tuple => (type * type) * (type * type) *)
     | Tuple(_, _), Tuple(_, _) ->  (
     		"(" ^ t_type_print type1 ^ ") * (" ^ t_type_print type2 ^ ")"
     	)
     
     (* Le tuple est uniquement à gauche => (type * type) * type			*)
     | Tuple(_, _), _ ->  "(" ^ t_type_print type1 ^ ") * " ^ t_type_print type2
     
     (* Le tuple est uniquement à droite => type * (type * type)			*)
     | _, Tuple(_, _) ->  t_type_print type1 ^ " * (" ^ t_type_print type2 ^ ")"
     	
     (* Cas de base, un tuple simple => type * type										*)
     | _, _ ->  t_type_print type1 ^ " * " ^ t_type_print type2
;;


let rec t_expr_print expression =
  match expression with
  | Var var -> var
  | Constante constante -> primitive_print constante
  | Fonction(name, type1, expression) -> (
  		"fun " ^ name ^ " : " ^ t_type_print type1 ^ " -> " ^ t_expr_print expression
  	)
  | Application(expression1, expression2) -> t_expr_print expression1 ^ " " ^ t_expr_print expression2
  | Tuple(expression1, expression2) -> "(" ^ t_expr_print expression1 ^ ", " ^ t_expr_print expression2 ^ ")"
  | Let(nom, type1, expression1, expression2) ->
     "let " ^ nom ^ " : " ^ t_type_print type1 ^ " = " ^ t_expr_print expression1 ^ " in " ^ t_expr_print expression2
;;



(* Question 3 *)

(* On créer notre environnement *)
type environement = (t_expr * t_type) list;;

(* On initialise notre environnement *)
let environement_construction =
    (Constante Un, Type Entier)
  ::(Constante Deux, Type Entier)
  ::(Constante Trois, Type Entier)
  ::(Constante Quatre, Type Entier)
  ::(Constante Cinq, Type Entier)
  ::(Constante A, Type Char)
  ::(Constante B, Type Char)
  ::(Constante C, Type Char)
  ::(Constante D, Type Char)
  ::(Constante Vrai, Type Bool)
  ::(Constante Faux, Type Bool)
  ::(Constante Plus, Fonction(TupleT(Type Entier, Type Entier), Type Entier))
  ::(Constante Moins, Fonction(TupleT(Type Entier, Type Entier), Type Entier))
  ::(Constante Inferieur, Fonction(TupleT(Type Entier, Type Entier), Type Bool))
  ::(Constante Superieur, Fonction(TupleT(Type Entier, Type Entier), Type Bool))
  ::(Constante Condition, Fonction(TupleT(Type Bool, TupleT(Type Entier, Type Entier)), T Entier))
  ::[]
;;

let rec environnement_print environnement =
  match environnement with
  | [] -> ()
  | tete::reste ->
     print_string (t_expr_print (fst tete) ^ " : " ^ (t_type_print (snd tete)));
     print_newline();
     environnement_print reste;                                                    
;;


(*** Question 4 ***)

let rec verif_type environnement expression =
  match expression with
  | Var valeur -> match (List.assoc_opt (Var valeur) environnement) with
                  |None -> failwith ("Variable [" ^ valeur ^ "] inexistante dans l'environnement")
                  |Some(v) -> v
  | Constante cte  -> match (List.assoc_opt (Constante cte) environnement) with
                  |None -> failwith ("Variable [" ^ valeur ^ "] inexistante dans l'environnement")
                  |Some(c) -> c
  | F_local(v_local, t_local, exp_local) -> let env_local = (Var v_local, t_local)::environnement in
                                            Fbis_local(t_local, verif_type env_local exp_local)
  | A_local(exp1, exp2) ->
     begin
       match verif_type environnement exp1, verif_type environnement exp2 with
       |F2_local(t_local,tbis_local), myType ->
         if t_local == myType then t2
         else failwith ("type [" ^ t_type_print t1 ^ "] inexistant")
       |_ -> failwith ("erreur expression -> pas une fonction")
     end

  |Tuple (exp1, exp2) -> myTuple(verif_type environnement exp1, verif_type environnement exp2)
  |Let (v_local, t_local, exp1, exp2) ->
    if t_local == verif_type environnement exp1
    then
      let env_local = (Var v_local, t_local)::environnement in
      verif_type env_local exp2
    else failwith ("Erreur type -->" ^ t_type_print t_local)
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
