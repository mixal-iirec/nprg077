// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

let print v =
  try
    printf "%A\n" v
  with
    | e -> printf "%A\n" e.Message

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
print(
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))
)

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
print(
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))
)

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
print(
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))
)

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
print(
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))
)

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
print(
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))
)

