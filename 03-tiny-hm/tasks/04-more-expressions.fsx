// ----------------------------------------------------------------------------
// Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  // NOTE: Added three more kinds of expression from TinyML
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  // NOTE: Added type for functions (of single argument)
  | TyFunction of Type * Type

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

let rec substType (subst:Map<_, _>) ty = 
  match ty with
  | TyVariable v -> if (Map.containsKey v subst) then (substType subst subst[v]) else TyVariable v
  | TyBool -> TyBool
  | TyNumber -> TyNumber
  | TyList ty -> TyList (substType subst ty)
  | TyFunction (ty1, ty2) -> TyFunction ((substType subst ty1), (substType subst ty2))

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
  | (ty1, ty2)::_ -> failwithf "Cannot be solved (Type mismatch `%A` != `%A`)" ty1 ty2


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

// NOTE: You will need this helper in checking of Lambda and Application.
// It generates a new type variable each time you call 'newTypeVariable()'
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
  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

// Run both of the phases and return the resulting type
let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType subst typ
  typ


// NOTE: Using the above, you will end up with ugly random type variable
// names like '_a4' etc. You can improve this by collecting all the type
// variable names that appear in a type and substituting them with a 
// list of nice names. Useful bit of code to generate the substitution is:
//
//   Map.ofList [ for i, n in Seq.indexed ["_a4"; "_a5"] -> 
//     n, string('a' + char i) ]
//
// You would still need to write code to collect all type variables in a type.

let print f =
  try
    printf "%A\n" (f())
  with
    | e -> printf "%A\n" e.Message


// let x = 10 in x = 10
print (fun () ->
  Let("x", Constant 10, Binary("=", Variable "x", Constant 10))
  |> infer 
)

// let f = fun x -> x*2 in (f 20) + (f 1)
print (fun () ->
Let("f",
  Lambda("x", Binary("*", Variable("x"), Constant(2))),
  Binary("+", 
    Application(Variable("f"), Constant(20)),
    Application(Variable("f"), Constant(1)) 
  ))
|> infer
)

// fun x f -> f (f x)
print (fun () ->
Lambda("x", Lambda("f", 
  Application(Variable "f", Application(Variable "f", Variable "x"))))
|> infer
)

// fun f -> f f 
// This does not type check due to occurs check
print (fun () ->
Lambda("f", 
  Application(Variable "f", Variable "f"))
|> infer
)

// fun f -> f 1 + f (2 = 3) 
// This does not type check because argument of 'f' cannot be both 'int' and 'bool'
print (fun () ->
Lambda("f", 
  Binary("+",
    Application(Variable "f", Constant 1),
    Application(Variable "f", Binary("=", Constant 2, Constant 3))
  ))
|> infer
)
