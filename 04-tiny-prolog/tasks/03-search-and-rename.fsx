// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Variable v -> if (Map.containsKey v subst) then (substitute subst subst[v]) else Variable v
  | Atom a -> Atom a
  | Predicate (p, l) -> Predicate (p, substituteTerms subst l)


and substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  subst |> List.map (fun (n, t) -> n, substitute newSubst t)


and substituteTerms subst (terms:list<Term>) = 
  terms |> List.map (substitute subst)


let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> Some []
  | h1::t1, h2::t2 ->
      match unify h1 h2 with
      | Some s1 ->
        let t1 = substituteTerms (Map.ofList s1) t1
        let t2 = substituteTerms (Map.ofList s1) t2
        match unifyLists t1 t2 with
        | Some s2 ->
          let s1 = substituteSubst (Map.ofList s2) s1
          Some (s1 @ s2)
        | _ -> None
      | _ -> None
  | _ -> None

and unify t1 t2 = 
  match t1, t2 with 
  | Atom(a1), Atom(a2) when a1 = a2 -> Some []
  | Predicate(p1, l1), Predicate(p2, l2) when p1 = p2 -> unifyLists l1 l2
  | Variable (v), s | s, Variable (v) -> Some [v, s]
  | _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Variable v -> [v]
  | Atom a -> []
  | Predicate (p, l) -> l |> List.collect freeVariables


let withFreshVariables (clause:Clause) : Clause =
  let number = nextNumber()
  let free = freeVariables clause.Head @ (clause.Body |> List.collect freeVariables)
  let map = free |> List.map (fun n -> (n, Variable (n + number.ToString()))) |> Map.ofList

  rule (substitute map clause.Head) (substituteTerms map clause.Body)


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  program |> List.map withFreshVariables |> List.choose (fun clause -> (unify query clause.Head) |> Option.map(fun v -> clause, v))


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

let print v =
  try
    printf "%A\n" v
  with
    | e -> printf "%A\n" e.Message

// Generating fresh variables - repeated calls
// should append new number to all variable names
print(
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables
)

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

printf "--------\n"

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
print(
query family (Predicate("male", [Variable("X")]))
)

printf "--------\n"
// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
print(
query family (Predicate("father", [Variable("X"); Atom("William")]))
)
