// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
  | Atom("zero") -> Some 0
  | Predicate("succ", [Number n]) -> Some (n + 1)
  | _ -> None

let rec (|List|_|) term : option<list<Term>> = 
  match term with 
  | Atom("empty") -> Some []
  | Predicate("cons", [h; List tl]) -> Some (h::tl)
  | _ -> None

let rec formatTerm term = 
  match term with 
  | Number n -> string n
  | List(l) -> "[" + (l |> List.map formatTerm |> String.concat ", ") + "]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) -> p + "(" + (items |> List.map formatTerm |> String.concat ", ") + ")"

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


let rec solve program subst goals : seq<list<string * Term>> = seq {
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let submap = Map.ofList newSubst
        let newGoals = (goals @ clause.Body) |> substituteTerms submap
        let subst = subst |> substituteSubst submap
        yield! solve program (subst @ newSubst) newGoals

  | [] -> yield subst
}


let run program query = 
  printfn "query: %s" (formatTerm query)
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program [] [query] do
    printfn "Accepted:"
    let subst = subst |> List.filter (fun (n, _) -> Set.contains n vars)
    for var, term in subst do
      printfn "%s := %s" var (formatTerm term)
  printfn "--------------\n\n"
  

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now called using 'run')
run family (Predicate("father", [Variable("X"); Atom("William")]))
run family (Predicate("father", [Variable("X"); Variable("Y")]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  match n with
  | 0 -> Atom("zero")
  | n -> Predicate("succ", [num (n - 1)])

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [num 2; num 3; Variable("X")]))
run nums (Predicate("add", [num 2; Variable("X"); num 5]))
run nums (Predicate("add", [num 2; Variable("Y"); Variable("X")]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term = 
  match l with
  | [] -> Atom("empty")
  | v::l -> Predicate("cons", [v; makeList l])


// TinyProlog code to represent 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// TODO: Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [l1to4; l5to9; Variable "X"]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [l1to4; Variable "X"; l1to9]))

// Query: append(X, Y, [1..9])
// Return: 
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [Variable "Y"; Variable "X"; l1to9]))
