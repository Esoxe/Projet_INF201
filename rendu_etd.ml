type dimension = int;; (*restreint aux entiers strictement positifs*)

type case = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)
type vecteur = int * int * int;; (*restreint au triplet tels (i,j,k) tels que i+j+k=0*)

type couleur = Vert | Jaune | Rouge | Noir | Bleu | Marron (*Les couleurs des joueurs*)
               | Libre 
               | Code of string (*une chaine restreinte a 3 caracteres*);;

type case_option = None | Some of case;;

type case_coloree = case * couleur;;

type configuration = case_coloree list * couleur list * dimension;; (*sans case libre*)
          
type coup = Du of case * case | Sm of case list;;

let indice_valide (x:int) (dim:dimension) : bool =
  x >= -2*dim && x<= 2*dim;;

let est_case ((i,j,k):case):bool=
 (i+j+k=0);;

let associe (a:'a) (l:('a*'b) list) (defaut:'b):'b = defaut;;

(*A MODIFIER en Q2*)
let est_dans_losange ((i, j, k) : case)  (dim:dimension): bool =
abs(j)<dim+1 && abs(k)<dim+1;;          

(*A MODIFIER en Q3*) 
let est_dans_etoile ((i, j, k) : case) (dim:dimension) : bool = 
i+j+k = 0
;;
(*Q4*)
let rec tourner_case(m:int)((i,j,k):case):case = 
if m = 0 then (i,j,k)
else tourner_case (m-1) (-k,-i,-j)
;;
(*Q5*)
let translate ((c1,c2,c3):case) ((v1,v2,v3) : vecteur): case = 
(c1+v1,c2+v2,c3+v3)
;;
(*Q6*)
let diff_case ((c1,c2,c3):case) ((d1,d2,d3):case) : vecteur = 
(d1-c1,d2-c2,d3-c3)
;;
(*Q7*)
let sont_cases_voisines ((c1,c2,c3):case) ((d1,d2,d3):case) : bool = abs(d1-c1)+abs(d2-c2)+abs(d3-c3) = 2
;;
(*Q8*)
let pt_ent (x:float) : float =
  float_of_int(truncate x);;

let midDist_2pts (c1,c2,c3:case) (d1,d2,d3:case) : float*float*float =
  (((float_of_int(c1+d1))/.2.),((float_of_int(c2+d2))/.2.),((float_of_int(c3+d3))/.2.));;

let coord_pivot (c1,c2,c3:case) (d1,d2,d3:case) : case_opt =
  let i,j,k = midDist_2pts (c1,c2,c3:case) (d1,d2,d3:case) in
  if (c1=d1 || c2=d2 || c3=d3) && ((pt_ent i)=i && (pt_ent j)=j && (pt_ent k)=k)
  then Some((int_of_float(i),int_of_float(j),int_of_float(k)))
  else None
;;

(*Q9*)
let vec_et_dist ((c1,c2,c3):case)((d1,d2,d3):case):vecteur*int= match (c1,c2,c3,d1,d2,d3) with 
|c1,c2,c3,d1,d2,d3 when d1-c1 <> 0 -> let d = abs(d1-c1) in (((c1-d1)/d,(c2-d2)/d,(c3-d3)/d),d)
|c1,c2,c3,d1,d2,d3 when d2-c2 <> 0 -> let d = abs(d2-c2) in (((c1-d1)/d,(c2-d2)/d,(c3-d3)/d),d)
;;
(*Q10*)
let rec insere(liste : 'a list) (x : 'a): 'a list = 
match liste with 
|[] -> [x]
|pr::fin -> pr::insere fin x
;; 
let tourner_liste(liste1:'a list):'a list =  
match liste1 with 
|[] -> []
|pr::fin -> insere fin pr
;;
let rec der_list(liste : 'a list):'a = 
match liste with 
|[pr] -> pr
|pr::fin -> der_list fin
;;

(*Q11*)
let rec remplir_segment(m:int) ((i,j,k):case) : case list =
match m with 
|1 -> [(i,j,k)]
|_ -> (i,j,k):: remplir_segment (m-1) (i,j+1,k-1)
;;

(*Q12*)
let rec remplir_triangle_bas (m:int) ((i,j,k) : case) : case list = 
match m with 
|1 -> [(i,j,k)]
|_ -> (remplir_segment m (i,j,k))@remplir_triangle_bas (m-1) (i-1,j+1,k)
;;


(*AFFICHAGE (fonctionne si les fonctions au dessus sont remplies)*)
(*transfo transforme des coordonnees cartesiennes (x,y) en coordonnees de case (i,j,k)*)
let transfo x y = (y, (x-y)/2,(-x-y)/2);;

let couleur2string (coul:couleur):string =
  match coul with
  | Libre -> " . "
  | Code s -> s  
  | Vert -> " V "
  | Jaune -> " J "
  | Rouge -> " R "
  | Noir -> " N "
  | Bleu -> " B "
  | Marron -> " M ";;

let rec affiche_ligne (n:int) (m:int) (config:configuration) : string =
  let (lcc,_,dim)=config in
    if m = (4 * dim) + 1 then " " (*fin de ligne*)
    else
      let c = transfo m n in
      if not ((n+m) mod 2 = 0) || not (est_dans_etoile c dim) then (*ceci est une inter-case (case inutile d'un damier) ou hors de l'etoile*)
        "   "^ affiche_ligne n (m + 1) config
      else (*ceci est une case ou bien en dehors du plateau*)
       (couleur2string (associe c lcc Libre)) ^ affiche_ligne n (m + 1) config;;


let affiche (config:configuration):unit =
  let (_,_,dim)=config in
    let rec affiche_aux n =
      if n = - 2 * dim - 1 then ()
      else
      begin
      print_endline (affiche_ligne n (-4*dim-1) config);
      print_endline "\n";
      affiche_aux (n - 1)
      end
    in
    affiche_aux (2*dim+1);;

let conf_1=([((0,0,0),Jaune)],[Jaune],2);;
affiche conf_1;;
let conf_reggae=([((0,-1,1),Vert);((0,0,0),Jaune);((0,1,-1),Rouge)],[Vert;Jaune;Rouge],1);;
affiche conf_reggae;;
let conf_vide=([],[],2);;
affiche conf_vide;;


(*A essayer apres avoir fait remplir_init
affiche (remplir_init [Code "Ali";Code "Bob";Code "Jim"] 3);;
*)
 