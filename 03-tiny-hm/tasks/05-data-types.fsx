// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable v -> v = vcheck
  | TyBool -> false
  | TyNumber -> false
  | TyList ty -> occursCheck vcheck ty
  | TyFunction (ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2
  | TyTuple (ty1, ty2) -> occursCheck vcheck ty1 || occursCheck vcheck ty2

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyVariable v -> if (Map.containsKey v subst) then (substType subst subst[v]) else TyVariable v
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList ty -> TyList (substType subst ty)
  | TyFunction (ty1, ty2) -> TyFunction ((substType subst ty1), (substType subst ty2))
  | TyTuple (ty1, ty2) -> TyTuple ((substType subst ty1), (substType subst ty2))

let substConstrs subst cs = 
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
  | (TyFunction (f1ty1, f1ty2), TyFunction (f2ty1, f2ty2))::cs ->
    solve ((f1ty1, f2ty1)::(f1ty2, f2ty2)::cs) 
  | (TyTuple (f1ty1, f1ty2), TyTuple (f2ty1, f2ty2))::cs ->
    solve ((f1ty1, f2ty1)::(f1ty2, f2ty2)::cs) 
  | (ty1, ty2)::_ -> failwithf "Cannot be solved (Type mismatch `%A` != `%A`)" ty1 ty2


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> TyNumber, []
  | Binary("+", e1, e2)
  | Binary("*", e1, e2) ->
    let t1, s1 = generate ctx e1
    let t2, s2 = generate ctx e2
    TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("=", e1, e2) ->
    let t1, s1 = generate ctx e1
    let t2, s2 = generate ctx e2
    TyBool, s1 @ s2 @ [ t1, t2 ]
  | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op
  | Variable v -> 
    if not (Map.containsKey v ctx) then failwithf "Variable %A not defined" v
    ctx[v], []
  | If(econd, etrue, efalse) ->
    let t1, s1 = generate ctx econd
    let t2, s2 = generate ctx etrue
    let t3, s3 = generate ctx efalse
    t2, s1 @ s2 @ s3 @ [ t1, TyBool; t2, t3 ]
  | Lambda(v, e) ->
      let ty_arg = newTyVariable()
      let ctx' = Map.add v ty_arg ctx
      let t1, s1 = generate ctx' e
      TyFunction (ty_arg, t1), s1
  | Application(e1, e2) -> 
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      let ty_res = newTyVariable()
      ty_res, s1 @ s2 @ [t1, TyFunction(t2, ty_res)]
  | Let(v, e1, e2) -> generate ctx (Application(Lambda(v, e2), e1))
  | Tuple(e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2
  | TupleGet(b, e) ->
      let t, s = generate ctx e
      let ty_res = newTyVariable()
      let ty_bin = newTyVariable()
      ty_res, s @ [t, (if b then TyTuple(ty_res, ty_bin) else TyTuple(ty_bin, ty_res))]

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType subst typ
  typ

let print f =
  try
    printf "%A\n" (f())
  with
    | e -> printf "%A\n" e.Message

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
print (fun () ->
etup |> infer
)
print (fun () ->
TupleGet(true, etup) |> infer
)
print (fun () ->
TupleGet(false, etup) |> infer
)

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
print (fun () ->
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer
)

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
print (fun () ->
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer
)


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
print (fun () ->
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
)
