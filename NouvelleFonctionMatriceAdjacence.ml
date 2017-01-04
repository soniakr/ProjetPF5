let adjacences_voronoi v m = let b = 
  matrice_adj (Array.length(v.seeds)) (Array.length(v.seeds)) and     
  compareMatchingAffect x y b i j= match x,y with  (* Compare deux éléments et affecte la valeur dans un tableau passé en argument (b) aux indices (i,j) si x et y sont différents *)    
  |x,y when x<>y -> (b.(i).(j)<- true) |x,y -> () and     
  safeBoundsPlus i size= match i with |i when (i<size) -> true |_-> false and 
  safeBoundsInf i= match i with |i when (i>0) -> true |_-> false and  
  taille = Array.length(m) in   
  for i= 0 to (taille-1) do       
  for j= 0 to (taille-1) do   
  let enCours = m.(i).(j) in (* Seed "h" *)  
  if (safeBoundsPlus i taille) then compareMatchingAffect enCours m.(i+1).(j) b enCours m.(i+1).(j) else ();
  if (safeBoundsPlus j taille) then compareMatchingAffect enCours m.(i).(j+1) b enCours m.(i).(j+1) else ();            
  if (safeBoundsInf i) then compareMatchingAffect enCours m.(i-1).(j) b enCours m.(i-1).(j) else (); 
  if (safeBoundsInf j) then compareMatchingAffect enCours m.(i).(j-1) b enCours m.(i).(j-1) else ();
  done;  
  done;   
  b;;
