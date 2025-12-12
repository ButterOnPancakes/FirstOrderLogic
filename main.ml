(* Placeholders *)
type variable = string
type fonction = string;;
let arite (f : fonction) : int = 0;;

type terme = 
  |Var of variable
  |Const of fonction
  |Fonct of fonction * (terme list)
;;

let rec verif_arite = function
  |Var _ -> true
  |Const f -> arite f = 0
  |Fonct (f, l) -> arite f = List.length l && List.for_all verif_arite l
;;

let rec est_clos = function
  |Var _ -> false
  |Const _ -> true
  |Fonct (_, l) -> List.for_all est_clos l
;;

let rec substituer_terme (s : (variable * terme) list) = function
  |Var x -> begin
      match List.assoc_opt x s with
        |None -> Var x
        |Some t' -> t'
  end
  |Const f -> Const f
  |Fonct (f, l) -> Fonct (f, List.map (fun e -> substituer_terme s e) l)
;;

let rec appear_in (x : variable) = function
  |Var y when y = x -> true
  |Var _ -> false
  |Const _ -> false
  |Fonct (_, l) -> List.exists (appear_in x) l
;;
