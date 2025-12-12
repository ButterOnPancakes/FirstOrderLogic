let ex1 = [(
  Fonct ("f", [Var "x"; Fonct ("h", [Fonct ("h", [Var "b"])])]),
  Fonct ("f", [Fonct ("f", [Var "a"]); Fonct ("h", [Var "y"])])
)];;

let ex2 = [(Var "x", Fonct ("f", [Var "x"]))];;

let ex3 = [(
  Fonct ("g", [Var "x"; Fonct ("h", [Var "x"])]),
  Fonct ("g", [Var "a"; Var "a"])
)];;

let ex4 = [(Fonct ("+", [Var "x"; Const "1"]), Const "0")];;

let ex5 = [(
  Fonct ("+", [Fonct ("f", [Var "x"]); Fonct ("g", [Var "a"; Var "b"])]),
  Fonct ("+", [Var "y"; Fonct ("g", [Fonct ("h", [Const "1"]); Const "3"])])
)]

exception Non_Unifiable
let unification (l : (terme * terme) list) : ((variable * terme) list)=
  let rec aux substitution = function
    |[] -> substitution 
    |(u, v)::t when u = v -> aux substitution t
    |(Fonct (f1, l1), Fonct (f2, l2))::t when f1 = f2 ->
        aux substitution ((List.map2 (fun u v -> (u, v)) l1 l2)@t)
    |(Var x, u)::t when not (appear_in x u) -> aux ((x, u)::substitution) (List.map (fun (a, b) -> (substituer_terme [(x, u)] a, substituer_terme [(x, u)] b)) t)
    |(u, Var x)::t -> aux substitution ((Var x, u)::t)

    |(Const _, Const _)::t -> raise Non_Unifiable
    |(Fonct _, Const _)::t -> raise Non_Unifiable
    |(Const _, Fonct _)::t -> raise Non_Unifiable
    |(Fonct _, Fonct _)::t -> raise Non_Unifiable
    |(Var x, u)::t -> raise Non_Unifiable
  in aux [] l
;;
