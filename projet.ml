(********************************************************************
* 	PROJET : "ALGORITHME POUR LES JEUX: STRATEGIE MIN-MAX"
*	AUTEURS : 	Sangbé Narcisse SIDIBE, Groupe 2
*				William FLEURQUIN, Groupe 1
* 	FORMATION: Licence 3 Informatique
*********************************************************************)

(* ============================================================== *)	
(* ---- 					Type liste 							  *)	
(* ============================================================== *)

	type 'a t_liste = 'a list;;

	let l_cv() : 'a t_liste = [];;

	let l_ape(x,l): 'a t_liste = x::l;;

	let rec l_ade(x,l): 'a t_liste = match l with
	[] -> [x]
	| y::l1 -> y::l_ade(x,l1);;

	let l_ev(l: 'a t_liste) = match l with
	[] -> true
	|_ -> false;;

	let l_pe(l: 'a t_liste) = match l with
	[] -> failwith "erreur fonction l_pe, pile vide"
	| x::_ -> x;;

	let l_spe(l : 'a t_liste): 'a t_liste = match l with
	[] -> failwith "erreur fonction l_spe, pile vide"
	| x::ll -> ll;;

	let rec l_de(l: 'a t_liste) = match l with
	[] -> failwith "erreur fonction l_de, pile vide"
	| [x] -> x
	| _::ll -> l_de(l);;

	let rec l_sde(l : 'a t_liste):'a t_liste = match l with
	[] -> failwith "erreur fonction l_sde, pile vide"
	| [x] ->[]
	| x::ll -> x::l_sde(ll);;

(* ============================================================== *)	
	(* --- Fonction qui concatene deux listes *)
	let rec concat(l1,l2) = match l1 with
		|[] -> l2
		|x::l -> x::(concat(l,l2))
	;;
(* ============================================================== *)	
	(* --- fonction de conversion *)
	
	let convbis(l) =
		let n = List.length l in
			let rec convbis1(l,i,n,b) =
				if i > n
				then b
				else
					begin
					Bytes.set b i (l_pe(l));
					convbis1(l_spe(l),i+1,n,b)
					end
			in
				Bytes.to_string(convbis1(l,0,n-1,Bytes.create n));;


(* ============================================================== *)
(* ---- 				Type configuration 						  *)
(* ============================================================== *)

	type t_configuration = bool * int;;

	(* --- Choix du joueur *)
	let estJoueur1(c) = match c with
		(true,_) -> true
		|_ -> false
	;;
	
	(* --- Fonction qui retourne le booleen *)
	let getBooleen(c) = match c with
		(b,_) -> b
		|_ -> failwith"erreur : ce n'est pas une configuration valide"
	;;
	
	(* --- Nombre d'allumette(s) restante(s) *)
	let nbAllumetteRestantes(c) = match c with
		(_,x) -> x
		|_ -> failwith"erreur : ce n'est pas une configuration valide"
	;;	
	
	
	(* ============================================================== *)	
	(* --- fonction qui affiche une configuration *)
	let affiche_configuration(c) =
		let rec affiche(nb,l) =
			if(nb = 0)
			then l
			else
				let l1 = concat(l,[' ';'|']) in				
				affiche(nb-1,l1)
		in
		let l = ['a';'u';' ';'j';'o';'u';'e';'u';'r';' '] 
		and nb = nbAllumetteRestantes(c) in		
		if (estJoueur1(c)) then
			let l1 =concat(l,['1';' ';':'])
			in convbis(affiche(nb,l1))
		else
			let l2 = concat(l,['2';' ';':'])
			in convbis(affiche(nb,l2))		
	;;

	(* ============================================================== *)
	(* --- 				Arbre de configuration 						  *)
	(* ============================================================== *)

	type t_arbre_configuration =  A of t_configuration * float * t_arbre_configuration list;;
	
	(* --- CONSTRUCTEURS *)
	let a_cree(c,q,l) = A(c,q,l);;
	
	(* --- SELECTEURS : fonctions de manipulation *)
	
	let getConfiguration(a) = match a with
		A(c,_,_) -> c
		|_ -> failwith"erreur : il n'y a pas de configuration"
	;;
	
	let getQualite(a) = match a with
		A(_,q,_) -> q
		|_ -> failwith"erreur : il n'y a pas de qualite"
	;;
	
	let getListeSousArbres(a) = match a with
		A(_,_,l) -> l
		|_ -> failwith"erreur : il n'y a pas de listes"
	;;
	
	(*  --- FOnction qui calcule le nbre de successeurs *)
	let nbSucc(a) =
		if(l_ev(getListeSousArbres(a)))
		then 0		
		else
			if(nbAllumetteRestantes(getConfiguration(a)) = 1)
			then 0
			else
				if(nbAllumetteRestantes(getConfiguration(a)) = 2)
				then 1
				else
					if(nbAllumetteRestantes(getConfiguration(a)) = 3)
					then 2
					else 3
	;;
	
	(* --- Fonction qui retourne le 1er successeur *)
	let succ1(a) =
		if(nbSucc(a) = 0)
		then failwith"il n'y a pas d'arbre successeur"
		else
			l_pe(getListeSousArbres(a))
	;;

	(* --- Fonction qui retourne le 2e successeur *)
	let succ2(a) =
		if(nbSucc(a) > 1) then
			let l = getListeSousArbres(a) in
			l_pe(l_spe(l))
		else
			failwith"il n'y a pas de deuxieme arbre successeur"
	;;

	(* --- Fonction qui retourne le 3e successeur *)
	let succ3(a) = 
		if(nbSucc(a) > 2) then
			let l = getListeSousArbres(a) in
			l_pe(l_spe(l_spe(l)))
		else
			failwith"il n'y a pas de troisieme arbre successeur"
	;;


	(* ============================================================== *)
	(* --- Fonction qui crée un arbre à partir d'une configuration *)
				
	let rec creerArbre_aux(b, n, q) = 
		if(n<1)
		then (false, a_cree((false, 0), 0., l_cv()))
		else
			if(n=1) then
				let a1 = a_cree((b,n), q, l_cv()) in
				(true, a1)
			else
				if(n=2) then
					let f = a_cree((not(b),(n-1)), q, l_cv()) in
					let a2 = a_cree((b,n), q, l_ade(f, l_cv())) in
					(true, a2)
				else
					if(n=3) then						
						let f1 = a_cree((not(b), (n-1)), q, l_ade((a_cree((b, (n-2)), q, l_cv())), l_cv()))
						and f2 = a_cree((not(b), (n-2)), q, l_cv())
						in let l = l_ade(f2, l_ade(f1, l_cv()))
						in let a3 =  a_cree((b,n), q, l)
						in (true, a3)
					else
						let (b1, g) = creerArbre_aux(not(b), (n-1), q)
						and (b2, m) = creerArbre_aux(not(b), (n-2), q)
						and (b3, d) = creerArbre_aux(not(b), (n-3), q)
						in
						if(b1) then
							if(b2) then
								if(b3) then
									(true, a_cree((b,n), q, l_ape(g, l_ape(m, l_ape(d, l_cv())))))
								else
									(true, a_cree((b,n), q, l_ape(g, l_ape(m, l_cv()))))
							else
								if(b3) then
									(true, a_cree((b,n), q, l_ape(g, l_ape(d, l_cv()))))		
								else
									(true, a_cree((b,n), q, l_ape(g, l_cv())))		
						else
							if(b2) then
								if(b3) then
									(true, a_cree((b,n), q, l_ape(m, l_ape(d, l_cv()))))	
								else
									(true, a_cree((b,n), q, l_ape(m, l_cv())))	  
							else
								if(b3) then
									(true, a_cree((b,n), q, l_ape(d, l_cv())))
								else
									(false, a_cree((false, 0), 0., l_cv()))
	;;

	let creerArbre(c) = 					
		let (b, a) = creerArbre_aux(getBooleen(c), nbAllumetteRestantes(c), 1.)	in
		if(b)
		then a		
		else failwith"configuration incompatible"
	;;
	
	(* ============================================================== *)
	(* --- Fonction qui crée un arbre evalué à partir d'un arbre de configuration *)		
	
	
	(* Fonction d'evaluation une configuration *)
	let evaluer(c) = 
		if(nbAllumetteRestantes(c) = 1)
		then 0.
		else 1.
	;;
	
	let rec creerArbreEval(a) = 
		let c = getConfiguration(a) and n = nbSucc(a) in
		if(n = 0)
		then a_cree(c, evaluer(c), l_cv())
		else
			if(n=1) then
				let f = creerArbreEval(succ1(a))
				in a_cree(c, evaluer(c), l_ade(f, l_cv()))
			else
				if(n=2) then
					let f1 = creerArbreEval(succ1(a))
					and f2 = creerArbreEval(succ2(a)) in
						let l = l_ape(f1, l_ape(f2, l_cv()))
						in a_cree(c, evaluer(c), l)
				else
					let g = creerArbreEval(succ1(a))
					and m = creerArbreEval(succ2(a))
					and d = creerArbreEval(succ3(a)) in
						let l1 = l_ape(g, l_ape(m, l_ape(d, l_cv())))
						in a_cree(c, evaluer(c), l1)
	;;


	(* ============================================================== *)
	(* --- Fonction qui crée un arbre evalué à partir d'une configuration *)		
	
	let rec creerArbreEvalBis_aux(b, n) = 
		if(n < 1)
		then (false, a_cree((b,n), 0., l_cv()))
		else
			if(n=1) then
				let a1 = a_cree((b,n), evaluer((b,n)), l_cv()) in
				(true, a1)
			else
				if(n=2) then
					let f = a_cree((not(b),(n-1)), evaluer((not(b),(n-1))), l_cv()) in
					let a2 = a_cree((b,n), evaluer((b,n)), l_ade(f, l_cv())) in
					(true, a2)
				else
					if(n=3) then		
						let f11 = a_cree((b, (n-2)), evaluer((b,(n-2))), l_cv()) in		
							let f1 = a_cree((not(b), (n-1)), evaluer((not(b),(n-1))), l_ade(f11, l_cv()))
							and f2 = a_cree((not(b), (n-2)), evaluer((not(b),(n-2))), l_cv())
							in let l = l_ade(f2, l_ade(f1, l_cv()))
							in let a3 =  a_cree((b,n), evaluer((b,n)), l)
							in (true, a3)
					else
						let (b1, g) = creerArbreEvalBis_aux(not(b), (n-1))
						and (b2, m) = creerArbreEvalBis_aux(not(b), (n-2))
						and (b3, d) = creerArbreEvalBis_aux(not(b), (n-3))
						in
						if(b1) then
							if(b2) then
								if(b3) then
									(true, a_cree((b,n), evaluer((b,n)), l_ape(g, l_ape(m, l_ape(d, l_cv())))))
								else
									(true, a_cree((b,n), evaluer((b,n)), l_ape(g, l_ape(m, l_cv()))))
							else
								if(b3) then
									(true, a_cree((b,n), evaluer((b,n)), l_ape(g, l_ape(d, l_cv()))))		
								else
									(true, a_cree((b,n), evaluer((b,n)), l_ape(g, l_cv())))		
						else
							if(b2) then
								if(b3) then
									(true, a_cree((b,n), evaluer((b,n)), l_ape(m, l_ape(d, l_cv()))))	
								else
									(true, a_cree((b,n), evaluer((b,n)), l_ape(m, l_cv())))	  
							else
								if(b3) then
									(true, a_cree((b,n), evaluer((b,n)), l_ape(d, l_cv())))
								else
									(false, a_cree((false, 0), 0., l_cv()))
	;;		 


	let creerArbreEvalBis(c) = 
		let (b, a) = creerArbreEvalBis_aux(getBooleen(c), nbAllumetteRestantes(c)) in
		if(b)
		then a
		else failwith"configuration incompatible"
	;;


	(* ============================================================== *)
	(* --- Fonction qui calcule le coup suivant en appliquant la stratégie min-max *)		

	(* --- Fonction max et min *)
	let max(x, y) = if(x > y) then x else y;;
	let min(x, y) = if(x > y) then y else x;;

	
	(* Fonction d'évaluation qui maximise *)
	let rec evalA(a) =
		if(nbSucc(a) = 0)
		then 1.
		else
			if(nbSucc(a) = 1)
			then evalB(succ1(a))
			else
				if(nbSucc(a) = 2)
				then max(evalB(succ1(a)), evalB(succ2(a)))
				else
					max(evalB(succ1(a)), max(evalB(succ2(a)), evalB(succ3(a))))
	and
	(* Fonction d'évaluation qui minimise *)
	evalB(a) = 
		if(nbSucc(a) = 0)
		then 0.
		else
			if(nbSucc(a) = 1)
			then evalA(succ1(a))
			else
				if(nbSucc(a) = 2)
				then min(evalA(succ1(a)), evalA(succ2(a)))
				else
					min(evalA(succ1(a)), min(evalA(succ2(a)), evalA(succ3(a))))
	;;

	(* Fonction qui donne le coup à jouer *)
	let coupAjouer(c) = 
		if(nbAllumetteRestantes(c) > 0) then
			let a = creerArbreEvalBis(c) in
			let n = nbSucc(a) in
				if(nbSucc(a) = 1)
				then getConfiguration(succ1(a))
				else
					if(n = 2) then
						if(evalB(succ1(a)) > evalB(succ2(a)))
						then getConfiguration(succ1(a))
						else getConfiguration(succ2(a))
					else
						if(evalB(succ1(a)) > evalB(succ2(a)))
						then
							if(evalB(succ1(a)) > evalB(succ3(a)))
							then getConfiguration(succ1(a))
							else getConfiguration(succ2(a))
						else
							if(evalB(succ2(a)) > evalB(succ3(a)))
							then getConfiguration(succ2(a))
							else getConfiguration(succ3(a))
		else failwith"configuration incompatible"
	;;


	(* ============================================================== *)
	(* Fonction qui permet de jouer contre l'ordinateur *)
	let rec jeuVsOrdi(c) = 
		begin
			print_string(affiche_configuration(c));
			print_newline();
			if(nbAllumetteRestantes(c) = 1) then
				if(estJoueur1(c)) then
					convbis(['O' ; 'R' ; 'D' ; 'I' ; ' ' ; 'A' ; ' ' ; 'G' ; 'A' ; 'G' ; 'N' ; 'E'])
				else 
					convbis(['T' ; 'U' ; ' ' ; 'A' ; 'S' ; ' ' ; 'G' ; 'A' ; 'G' ; 'N' ; 'E'])
			else
				if(estJoueur1(c)) then
					begin
						print_string" Nbre d'allumettes à enlever : ";
						let n = read_int() in
							if((n < 1) || (n > 3) || (n  > nbAllumetteRestantes(c))) then
								jeuVsOrdi(c)
							else
								if((nbAllumetteRestantes(c) - n) < 1)
								then convbis(['T' ; 'U' ; ' ' ; 'A' ; 'S' ; ' ' ; 'P' ; 'E' ; 'R' ; 'D' ; 'U'])
								else 
									let b = getBooleen(c) and n1 = nbAllumetteRestantes(c)
									in jeuVsOrdi(not(b), (n1-n))
					end
				else
					let cpOrdi = coupAjouer(c) in
						begin
							print_string " Ordi a enlevé : ";
							print_int (nbAllumetteRestantes(c) - nbAllumetteRestantes(cpOrdi));
							print_string " allumette(s) ";
							print_newline();
							jeuVsOrdi(cpOrdi)
						end
		end
	;;

	

	(* ============================================================== *)
	(* Fonction qui permet à l'ordinateur de jouer contre lui-même *)	

	let rec jeuOrdiVsOrdi_aux(c) = 
		begin
			print_string(affiche_configuration(c));
			print_newline();
			if(nbAllumetteRestantes(c) = 1) then
				if(estJoueur1(c)) then
					convbis(['J' ; '2' ; ' ' ; 'G' ; 'A' ; 'G' ; 'N' ; 'E'])
				else 
					convbis(['J' ; '1' ; ' ' ; 'G' ; 'A' ; 'G' ; 'N' ; 'E'])
			else
				begin
					let c2 = coupAjouer(c) in
						begin
							if(estJoueur1(c)) then								
								print_string " J1 a enlevé : "								
							else
								print_string " J2 a enlevé : " ;
							print_int (nbAllumetteRestantes(c) - nbAllumetteRestantes(c2));
							print_string " allumettes ";
							print_newline();
							jeuOrdiVsOrdi_aux(c2)
						end
				end
		end
	;;

	let jeuOrdiVsOrdi(c) = 
		if(nbAllumetteRestantes(c) > 0)
		then jeuOrdiVsOrdi_aux(c)
		else failwith"configuration incompatible"
	;;
	