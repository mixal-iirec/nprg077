// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  // NOTE: Added 'Call' as a special kind of predicate
  | Call of Term * Term list

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
  | Call(c, l) -> Call (substitute subst c, substituteTerms subst l)


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

  | Predicate(p1, args1), Call (Predicate (p2, args2a), args2b)
  | Call (Predicate (p2, args2a), args2b), Predicate(p1, args1)
    when p1 = p2 -> unifyLists args1 (args2a @ args2b)
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
  | Call(c, args) -> formatTerm (Predicate ("call", c::args))

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
  | Call (c, l) -> freeVariables c @ (l |> List.collect freeVariables)


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
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n = 
  match n with
  | 0 -> Atom("zero")
  | n -> Predicate("succ", [num (n - 1)])

let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term = 
  match l with
  | [] -> Atom("empty")
  | v::l -> Predicate("cons", [v; makeList l])

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

// ----------------------------------------------------------------------------
// Call and maplist
// ----------------------------------------------------------------------------

// The Prolog 'call' operation takes a term and a list of arguments
// and supplies the arguments as additional arguments to the term.
// So, for example, calling 'call(add(1), 2, X)' becomes 'add(1, 2, X)'
run nums (Call(Predicate("add", [num 1]), [num 2; Variable "X"]))
run nums (Call(Predicate("add", [num 1; Variable "X"]), [num 5]))

// This can be used to implement the 'maplist' function:
// $ maplist(_, [], []).
// $ maplist(G,[X|Xs],[Y|Ys]) :- maplist(G,Xs,Ys), call(G,X,Y).
let maplist = [
  fact (Predicate("maplist", [ Variable("_"); Atom("empty"); Atom("empty") ]))
  rule (Predicate("maplist", [ 
    Variable("G")
    Predicate("cons", [ Variable("X"); Variable("Xs") ])
    Predicate("cons", [ Variable("Y"); Variable("Ys") ]);  
  ])) [
    Predicate("maplist", [ Variable("G"); Variable("Xs"); Variable("Ys") ])
    Call(Variable("G"), [ Variable("X"); Variable("Y") ])
  ]
]

// Query: maplist(add(10), l1to9, Y)
// Returns: Y -> [11; 12; ..; 19]
run (nums @ maplist) (Predicate("maplist", 
  [ Predicate("add", [num 10]); l1to9; Variable("Y") ]))
