#load "graphics.cma";;
open Graphics;;

type seed = {c : color option; x : int; y : int}
type voronoi = {dim : int * int; seeds : seed array}
type point = { x:float; y:float };;

let matrice n p = Array.make_matrix n p 0;; 
let matrice_adj a b = Array.make_matrix a b false;;


let v1 = {dim = 600,600;
seeds = [|
{c=None; x=100; y=100}; {c=Some red; x=125; y=550};{c=Some green; x=300; y=300};
{c=Some blue; x=150; y=250}; {c=None; x=250; y=300}; {c=None; x=300; y=500};
{c=Some red; x=400; y=100}; {c=None; x=450; y=450}; {c=None; x=500; y=250};
{c=Some yellow; x=575; y=350}; {c=None; x=75; y=470}; {c=None; x=250; y=50};
|]
}

(*Retourne l'abscisse ou l'ordonnée d'un point*)
let get_element_couple a i = 
	match a with 
	| (x,y) -> if i=0 then x else y
	;;

let valeur_absolue x =
	if x>0 then x 
	else (-x)
;; 

(*Calcule la distance Taxicab entre deux points a et b*)
let distance_taxicab a b =
	let res1 = (get_element_couple a 0) - (get_element_couple b 0) in
			let res2 = (get_element_couple a 1) - (get_element_couple b 1) in 
					(valeur_absolue res1) + (valeur_absolue res2)
	;;
	

(*Calcule la distance Euclidienne entre deux points a et b
let distance_euclide a b =
	let res1 = ((get_element_couple a 0) - (get_element_couple b 0))**2 in
			let res2 = ((get_element_couple a 1) - (get_element_couple b 1))**2 in 
						sqrt(res1 + res2)
	;; *)


(*Cacule la matrice m des régions du diagramme*)
let regions_voronoi distance v =
		let m = matrice (get_element_couple v.dim 0) (get_element_couple v.dim 1) in
			for i = 0 to ((get_element_couple v.dim 0)-1) do
				for j = 0 to ((get_element_couple v.dim 1)-1) do 
					let min = ref ( distance (i,j) (v.seeds.(0).x , v.seeds.(0).y) ) in (* Variable qui change à chaque fois est une ref , On suppose que la distance min est calculé avec le seed num 0  *)
					let indice_germe = ref (0) in
					for k =1 to (Array.length(v.seeds)-1) do 
						let n = distance (i,j) (v.seeds.(k).x , v.seeds.(k).y) in
						if (n<(!min)) then begin min:=n ; indice_germe:=k ; end
					done;
					m.(i).(j) <- (!indice_germe) ;
				done;
			done;
			m		
	;;

(*Colorier un pixel de coordonnées (x,y)*)
let colorier_pixel x y color=
	set_color(color);
	plot x y 
	;;

(*Retourne la couleur d'une région d'une diagramme*)
let get_couleur c = 
	match c with 
	| None -> white 
	| Some couleur -> couleur
	;;

(*Affichage du diagramme*)
let draw_voronoi m v = 
	 open_graph " 600x600";
		for i = 0 to (Array.length(m)-1) do
			for j=0 to (Array.length(m)-1) do
				
				 if( (i>= 1 && m.(i).(j) <> m.(i-1).(j)) 
													|| ( i <(Array.length(m)-1) && m.(i).(j) <> m.(i+1).(j)) 
													|| ( j>=1 && m.(i).(j) <> m.(i).(j-1)) 
          								|| ( j<(Array.length(m)-1) && m.(i).(j) <>  m.(i).(j+1))) then colorier_pixel i j black
				 else colorier_pixel i j (get_couleur((v.seeds.(m.(i).(j))).c));
			done;
		done
;;


(*Cacule de la matrice d'adjacence*)
let adjacences_voronoi v m =
		let b = matrice_adj (Array.length(v.seeds)) (Array.length(v.seeds)) in 
		for i= 0 to (Array.length(m)-1) do 
				for j= 0 to (Array.length(m)-1) do
						if ( i>= 1 && m.(i).(j) <> m.(i-1).(j)) then b.(m.(i).(j)).(m.(i-1).(j)) <- true;
						if ( i <(Array.length(m)-1) && m.(i).(j) <> m.(i+1).(j)) then b.(m.(i).(j)).(m.(i+1).(j)) <- true;
						if ( j>=1 && m.(i).(j) <> m.(i).(j-1)) then b.(m.(i).(j)).(m.(i).(j-1)) <- true;
          	if ( j<(Array.length(m)-1) && m.(i).(j) <>  m.(i).(j+1)) then b.(m.(i).(j)).(m.(i).(j+1)) <- true;
				done;
		done;			 
		b
;; 


(*Récupère coordonnèes du clique de la souris*)
let clique_souris() = 
		let e = wait_next_event[Button_down] in 
				let x = e.mouse_x and y = e.mouse_y in 
						print_int x ;
						print_string ",";
						print_int y ;
						print_newline();
	;;


(*let set_couleur c=
	set_color c 
;;

let change_color i =
	get_couleur(List.nth [Some red; Some yellow; Some blue; Some green] i)
;;
*)


(*La région en fonction d'un point de coordonées x y*)
let get_Zone x y m =
  m.(x).(y);
;;


let matchColor c = match c with 
|None -> white
|Some blue -> blue
|Some red -> red
|Some green -> green
|Some yellow -> yellow
;;

let get_Color i s =   
matchColor s.(i).c;
;;

(*Prochaine couleur*)
let next_Color c = 
  let color = ref black in
  if(c = red) then color := blue;
  if(c = blue) then color := green;
  if(c = green) then color := yellow;
  if(c = yellow) then color := white;
  if(c = white) then color := red;
  !color;
;;

(*Détermine la prochaine couleur pour colorier une région en fonction de sa couleur actuelle*)
let makeColorOption c = match c with
  | blue -> Some blue
  | red -> Some red
  | green -> Some green
  | white -> None
  | yellow -> Some yellow
;;


(*Recupère les coordonnées du pixel où on clique et colorie toute la région*)

let get_pixel_Color m v = 
	let e = wait_next_event[Button_down] in 
		let x = e.mouse_x and y =e.mouse_y in
 			 let zone = get_Zone x y m in
 			 let x_seed =  v.seeds.(zone).x in
		   let y_seed =  v.seeds.(zone).y in
			(* 						if (v.seeds.(k).c = None) then
Ajouter condition ne pas colorier une région précoloriée dans la carte*)
          let newColor = makeColorOption (next_Color (get_Color (zone) (v.seeds)) ) in
           v.seeds.(zone) <- ({c = newColor ; x = x_seed ; y = y_seed});  
           get_Color (zone) (v.seeds);
				   
;;


(*Change la couleur de la région correspondant au pixel x,y
let changer_couleur v m =
	let e = wait_next_event[Button_down] in 
		let x = e.mouse_x and y =e.mouse_y in
			let k = m.(x).(y) in 
						if (v.seeds.(k).c = None) then
							while(true) do
							v.seeds.(k)<- {c=makeColorOption(nextColor c); x=v.seeds.(k).x ;y=v.seeds.(k).y}	;
				 	    draw_voronoi m v;
							(*let event = wait_next_event[Button_down] in 
							x := event.mouse_x;
							y := event.mouse_y;*)
							done;	*)

(*Lancement du jeu *)
let boucle m v =
	while (true) do 
			get_pixel_Color m v;
			draw_voronoi m v; 
	done;
;;

(*Iniatialisation matrice des régions avec distance Taxicab*)
	let mat = regions_voronoi distance_taxicab v1;;

	draw_voronoi mat v1;;
	adjacences_voronoi v1 mat;;
	boucle mat v1;;
	
