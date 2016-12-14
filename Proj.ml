#load "graphics.cma";;
open Graphics;;

type seed = {c : color option; x : int; y : int}
type voronoi = {dim : int * int; seeds : seed array}
type point = { x:float; y:float };;

let matrice n p = Array.make_matrix n p 0;; 
let matrice_adj a b = Array.make_matrix a b false;;

let v ={ dim = (600,600) ;
			seeds = [| {c=None; x=100; y=150}; 
			{c=Some red; x=200; y=550};
			{c=Some green; x=350; y=300};
			{c=Some blue; x=450; y=350};
			{c = None; x=300; y=455};
      {c = None; x=260; y=10};
      {c = None; x=357; y=75};
			|] }

let v1 = {dim = 600,600;
seeds = [|
{c=None; x=100; y=100}; {c=Some red; x=125; y=550};{c=Some green; x=300; y=300};
{c=Some blue; x=150; y=250}; {c=None; x=250; y=300}; {c=None; x=300; y=500};
{c=Some red; x=400; y=100}; {c=None; x=450; y=450}; {c=None; x=500; y=250};
{c=Some yellow; x=575; y=350}; {c=None; x=75; y=470}; {c=None; x=250; y=50};
|]
}

let get_element_couple a i = 
	match a with 
	| (x,y) -> if i=0 then x else y
	;;

let valeur_absolue x =
	if x>0 then x 
	else (-x)
;; 

let distance_taxicab a b =
	let res1 = (get_element_couple a 0) - (get_element_couple b 0) in
			let res2 = (get_element_couple a 1) - (get_element_couple b 1) in 
					(valeur_absolue res1) + (valeur_absolue res2)
	;;
	

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

let colorier_pixel x y color=
	set_color(color);
	plot x y 
	;;

let get_couleur c = 
	match c with 
	| None -> white 
	| Some couleur -> couleur
	;;

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


matrice_adj 5 5;;

(*let adjacences_voronoi v m =
		let b = matrice_adj (Array.length(v.seeds)) (Array.length(v.seeds)) in 
		for i= 0 to (Array.length(m1)-1) do 
				for j= 0 to (Array.length(m1)-1) do
						if ( i>= 1 && m.(i).(j) <> m.(i-1).(j)) then b.(m(i).(j)).(m.(i-1).(j)) <- true;
						if ( i <(Array.length(m)-1) && m.(i).(j) <> m.(i+1).(j)) then b.(m(i).(j)).(m.(i+1).(j)) <- true;
						if ( j>=1 && m.(i).(j) <> m.(i).(j-1)) then b.(m(i).(j)).(m.(i).(j-1)) <- true;
          	if ( j<(Array.length(m)-1) && m.(i).(j) <>  m.(i).(j+1)) then b.(m(i).(j)).(m.(i).(j+1)) <- true;
				done;
		done;			 
		b
	;; 


	get_element_couple (3,4) 0 ;;
	distance_taxicab (3,8) (6,9) ;;
	let mat = regions_voronoi distance_taxicab v1;;
	draw_voronoi mat v1;;
	adjacences_voronoi v1 m;;
*)
