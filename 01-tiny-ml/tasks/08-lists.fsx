// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  // NOTE: A value that represents "empty value" and is
  // useful as the value for representing the empty list.
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  // NOTE: An expression that evaluates to a unit value.
  // This exists in F# too and it is written as '()'
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  let eval = fun e -> evaluate ctx e
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | "%" -> ValNum(n1 % n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      match eval e with 
      | ValNum n -> 
        match op with 
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
      | _ -> failwith "unary operator unsupported on closures"
  | If (e1, e2, e3) ->
      match eval e1 with 
      | ValNum n1 -> if n1 = 1 then eval e2 else eval e3
      | _ -> failwith "bool evaluation unsupported on non numbers"
  
  | Lambda(v, e) -> ValClosure (v, e, ctx)

  | Application(e1, e2) ->
      match eval e1 with 
      | ValClosure (v, e, ctx') ->
          let ctx' = Map.add v (lazy (eval e2)) ctx'
          evaluate ctx' e
      | _ -> failwith "unsupported closure application"

  | Let(v, e1, e2) -> evaluate ctx (Application(Lambda(v, e2), e1))
  | Tuple(e1, e2) -> ValTuple (eval e1, eval e2)
  | TupleGet(b, e) ->
      match eval e with
        | ValTuple (c1, c2) -> if b then c1 else c2
        | _ -> failwith "tuple access only supported on tuple"

  | Match(e, v, e1, e2) ->
      match eval e with
        | ValCase (b, v0) ->
          let ctx' = Map.add v (lazy v0) ctx
          let e' = if b then e1 else e2
          evaluate ctx' e'
        | _ -> failwith "matching without case unsupported"
  | Case(b, e) -> ValCase (b, eval e)
  | Recursive(v, e1, e2) -> 
      let rec ctx' = Map.add v (lazy (evaluate ctx' e1)) ctx
      evaluate ctx' e2
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
printf "%A" (evaluate Map.empty em)

// TODO: Can you implement 'List.filter' in TinyML too??
// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (map f) x#2) 
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//
let ef = 
  Recursive("filter",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        If (Application(Variable "f", TupleGet(true, Variable "x")),
            Case(true, Tuple(
              TupleGet(true, Variable "x"),
              Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
            )),
            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
        ),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "filter", 
      Lambda("y", If(Binary ("+", Variable "y", Constant -2), Constant 0, Constant 1))), el)
  )
printf "%A" (evaluate Map.empty ef)

let efm = 
  Recursive("filter",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        If (Application(Variable "f", TupleGet(true, Variable "x")),
            Case(true, Tuple(
              TupleGet(true, Variable "x"),
              Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
            )),
            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
        ),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "filter", 
      Lambda("y", If(Binary ("%", Variable "y", Constant 2), Constant 1, Constant 0))), el)
  )
printf "%A" (evaluate Map.empty efm)
