type predicat = string;;

type formule =
  |Pred of predicat * (terme list)
  |Exists of variable * formule
  |ForAll of variable * formule
  |Not of formule
  |And of formule * formule
  |Or of formule * formule
  |Imply of formule * formule
;;

let form1 = Exists ("x", And (Pred (">", [Var "x"; Const "0"]), ForAll ("y", (Pred ("<=", [Fonct ("*", [Var "x"; Var "x"]); Var "y"])))));;
let form2 = ForAll ("x", Pred ("<=", [Var "x"; Fonct ("+", [Var "x"; Var "t"])]));;
let form3 = Pred ("<=", [
  Fonct ("+", [Fonct ("*", [Var "x"; Var "x"]); Fonct ("*", [Var "y"; Var "y"])]);
  Fonct ("*", [Fonct ("+", [Var "x"; Var "y"]); Fonct ("+", [Var "x"; Var "y"])])
]);;
let form4 = And (
  ForAll ("x", Pred (">", [Fonct ("*", [Var "x"; Var "x"]); Const "0"])),
  Exists ("t", Pred ("<=", [Fonct ("+", [Var "x"; Const "1"]); Var "t"]))
);;
let form5 = Or (Pred ("P", [Var "x"]), Exists ("y", Pred ("Q", [Var "y"])));;
let form6 = Exists ("y", Or (Pred ("P", [Var "y"]), Pred ("Q", [Var "x"])));;
let form7 = Or (Pred ("P", [Var "x"]), Exists ("x", Pred ("Q", [Var "x"])));;
let form8 = Or (Pred ("pair", [Var "x"]), Pred ("pair", [Fonct ("+", [Var "x"; Const "1"])]));;
let form9 = Exists ("y", Pred ("=", [Var "y"; Fonct ("+", [Var "x"; Const "1"])]));;
let form10 = Imply (Pred ("<=", [Const "1"; Var "x"]), ForAll ("x", Pred ("<=", [Const "0"; Var "x"])));;

let sub1 = [("x", Var "z")];;
let sub2 = [("x", Var "y")];;
let sub3 = [("x", Var "x")];;
let sub4 = [("y", Var "z")];;
let sub5 = [("y", Var "x")];;
let sub6 = [("x", Fonct ("+", [Var "y"; Const "1"]))];;
let sub7 = [("x", Fonct ("*", [Var "x"; Var "x"]))];;
let sub8 = [("x", Const "1")];;

let variables_liees (f : formule) = 
  let rec aux quantifiees = function
    |Pred (_, l) -> List.filter (fun x -> List.exists (appear_in x) l) quantifiees
    |Exists (x, f) |ForAll (x, f) -> aux (x::quantifiees) f
    |Not f -> aux quantifiees f
    |And (f1, f2) |Or (f1, f2) |Imply (f1, f2) -> (aux quantifiees f1) @ (aux quantifiees f2)
  in List.sort_uniq compare (aux [] f)
;;

let variables_libres (f : formule) = 
  let rec terme_libres quantifiees libres = function
    |Var x when not (List.mem x quantifiees) -> x::libres
    |Var x -> libres
    |Const _ -> libres
    |Fonct (_, l) -> List.fold_left (fun acc t -> terme_libres quantifiees acc t) libres l
  in
  let rec aux quantifiees = function
    |Pred (_, l) -> List.fold_left (fun acc u -> terme_libres quantifiees acc u) [] l
    |Exists (x, f) |ForAll (x, f) -> aux (x::quantifiees) f
    |Not f -> aux quantifiees f
    |And (f1, f2) |Or (f1, f2) |Imply (f1, f2) -> (aux quantifiees f1) @ (aux quantifiees f2)
  in List.sort_uniq compare (aux [] f)
;;

let est_forme_prenexe (form : formule) =
  let rec aux flag = function
    |Pred _ -> true
    |Exists (_, f) |ForAll (_, f) -> flag && aux flag f
    |Not f -> aux false f
    |And (f1, f2) |Or (f1, f2) |Imply (f1, f2) -> (aux false f1) && (aux false f2)
  in aux true form
;;

let rec substituer_formule (s : (variable * terme) list) = function
  |Pred (p, l) -> Pred (p, List.map (substituer_terme s) l)
  |Not f -> Not (substituer_formule s f)
  |And (f1, f2) -> And (substituer_formule s f1, substituer_formule s f2)
  |Or (f1, f2) -> Or (substituer_formule s f1, substituer_formule s f2)
  |Imply (f1, f2) -> Imply (substituer_formule s f1, substituer_formule s f2)

  (* Possibilité d'erreur *)
  |Exists (x, f) -> 
      let libres = variables_libres f in
      let licite = List.for_all (fun (var, t) -> not ((appear_in x t) && (List.mem var libres))) s in
      if licite then Exists (x, substituer_formule (List.remove_assoc x s) f)
      else failwith "Pas Licite !"

  |ForAll (x, f) -> 
      let libres = variables_libres f in
      let licite = List.for_all (fun (var, t) -> not ((appear_in x t) && (List.mem var libres))) s in
      if licite then ForAll (x, substituer_formule (List.remove_assoc x s) f)
      else failwith "Pas Licite !"
;;
