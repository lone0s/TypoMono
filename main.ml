(**********************************************

	   Projet Programmation Fonctionnelle

				GHITA Alessandro
				GELINAUD Clement
				BENMOUSSATI Souhail

**********************************************)

(* QUESTION 1 : *)

(* Définition des types de base de mini-ML *)

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

(* Constantes et opérations *)
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

let expression = Let("decrement", Fonction(Type Entier, Type Entier),
	Fonction("x", Type Entier, Application(Constante Moins, Tuple(Var "x", Constante Un))),
	Tuple(Application(Var "decrement", Constante Un), Var "x"));;

t_expr_print(expression);;


(* Question 3 *)

(* On créer notre environnement *)
type environement = (t_expr * t_type) list;;

(* On initialise notre environnement *)
let environement_construction =
	(Constante Un, Type Entier)
	:: (Type Entier * Type Entier)
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
  ::(Constante Plus, Fonction(Tuple(Type Entier, Type Entier), Type Entier))
  ::(Constante Moins, Fonction(Tuple(Type Entier, Type Entier), Type Entier))
  ::(Constante Inferieur, Fonction(Tuple(Type Entier, Type Entier), Type Bool))
  ::(Constante Superieur, Fonction(Tuple(Type Entier, Type Entier), Type Bool))
  ::(Constante Condition, Fonction(Tuple(Type Bool, Tuple(Type Entier, Type Entier)), Type Entier))
  ::[]
;;

(* Affichage de l'environnement *)
let rec environnement_print environnement =
  match environnement with
  | [] -> ()
  | tete::reste ->
	 print_string (t_expr_print (fst tete) ^ " : " ^ (t_type_print (snd tete)));
	 print_newline();
	 environnement_print reste;
;;

(*** Question 4 ***)

(* Nous avons rencontrés des difficultés pour coder cette fonction,
		en particulier lors des essaies de la question 5,
		on obtiens : Exception: Failure "type [Entier * Entier] inexistant".

		Malgré nos efforts, nous n'avons pas su résoudre le(s) problème(s)
*)
let rec verif_type environnement expression =
  match expression with
  | Var valeur -> List.assoc (Var valeur) environnement
  | Constante cte  -> List.assoc (Constante cte) environnement
  | Fonction(v_local, t_local, exp_local) -> let env_local = (Var v_local, t_local)::environnement in
											Fonction(t_local, verif_type env_local exp_local)
  | Application(exp1, exp2) ->
	 begin
	   match verif_type environnement exp1, verif_type environnement exp2 with
	   |Fonction(t_local, tbis_local), myType ->
		 if t_local == myType then tbis_local
		 else failwith ("type [" ^ t_type_print t_local ^ "] inexistant")
	   |_ -> failwith ("erreur expression -> pas une fonction")
	 end

  |Tuple (exp1, exp2) -> Tuple(verif_type environnement exp1, verif_type environnement exp2)
  |Let (v_local, t_local, exp1, exp2) ->
	if t_local == verif_type environnement exp1
	then
	  let env_local = (Var v_local, t_local)::environnement in
	  verif_type env_local exp2
	else failwith ("Erreur type -->" ^ t_type_print t_local)
;;




(* QUESTION 5 : *)

(* Pour cette question, nous allons formatter
		l'expression à l'aide de sous-expressions
		afin de faciliter la lecture

		On rappelle qu'on veut obtenir l'expression :

		fun x : char -> (let succ : int -> int = fun x : int -> + (x, 1) in (succ 1, x))
*)

let expr_aux1 = Application(Constante Plus, Tuple(Var "x", Constante Un));;
let fonc_aux1 = Fonction("x", Type Entier, expr_aux1);;
let tuple_aux1 = Tuple(Application(Var "succ", Constante Un), Var "x");;
let let_aux1 = Let("succ", Fonction(Type Entier, Type Entier), fonc_aux1, tuple_aux1);;
let expression1 = Fonction("x", Type Char, let_aux1);;

(* On affiche l'expression *)
t_expr_print expression1;;

(* On vérifi son type *)
verif_type environement_construction expression1;;



(* Pour la deuxième expression, on souhaite obtenir :

fun x : char -> (let succ : int -> int = fun y : int -> + (y, 1) in (succ 1, x))

*)

let expr_aux2 = Application(Constante Plus, Tuple(Var "y", Constante Un));;
let fonc_aux2 = Fonction("y", Type Entier, expr_aux2);;
let tuple_aux2 = Tuple(Application(Var "succ", Constante Un), Var "x");;
let let_aux2 = Let("succ", Fonction(Type Entier, Type Entier), fonc_aux2, tuple_aux2);;
let expression2 = Fonction("x", Type Char, let_aux2);;

(* On affiche l'expression *)
t_expr_print expression2;;

(* On vérifi son type *)
verif_type environement_construction expression2;;