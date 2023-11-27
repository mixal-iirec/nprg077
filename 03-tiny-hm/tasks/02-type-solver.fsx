// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  match ty with
  | TyVariable v -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList ty -> occursCheck vcheck ty
 
let rec substType (subst:Map<string, Type>) ty = 
  match ty with
  | TyVariable v -> if (Map.containsKey v subst) then (substType subst subst[v]) else TyVariable v
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList ty -> TyList (substType subst ty)

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  List.map (fun (ty1, ty2) -> (substType subst ty1, substType subst ty2)) cs
 

let rec solve cs =
  match cs with 
  | [] -> Map.empty
  | (TyBool, TyBool)::cs -> solve cs
  | (TyNumber, TyNumber)::cs -> solve cs
  | (ty, TyVariable v)::cs
  | (TyVariable v, ty)::cs ->
    if occursCheck v ty then failwith "Cannot be solved (occurs check)"
    let cs = substConstrs (Map.ofList [(v, ty)]) cs
    let subst = solve cs
    let ty = substType subst ty
    Map.add v ty subst
  | (TyList l1, TyList l2)::cs -> solve ((l1, l2)::cs)
  | (ty1, ty2)::_ -> failwithf "Cannot be solved (Type mismatch `%A` != `%A`)" ty1 ty2


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
printf "%A\n" (solve 
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ])

// Cannot be solved (list<'a> <> bool)
try
  solve 
    [ TyList(TyVariable("a")), TyVariable("b")
      TyVariable("b"), TyBool ]
    |> ignore
with
  | e -> printf "%A\n" e.Message

// Can be solved ('a = number, 'b = list<number>)
printf "%A\n" (solve 
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ])

// Cannot be solved ('a <> list<'a>)
try
  solve 
    [ TyList(TyVariable("a")), TyVariable("a") ]
    |> ignore
with
  | e -> printf "%A\n" e.Message
