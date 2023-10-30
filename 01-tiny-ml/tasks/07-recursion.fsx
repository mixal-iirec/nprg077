// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value

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
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  let eval = fun e -> evaluate ctx e
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      match eval e1, eval e2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
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
          let e = if b then e1 else e2
          evaluate ctx' e
        | _ -> failwith "matching without case unsupported"
  | Case(b, e) -> ValCase (b, eval e)
  | Recursive(v, e1, e2) -> 
      let rec ctx' = Map.add v (lazy (evaluate ctx' e1)) ctx
      evaluate ctx' e2

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let er' = 
  Recursive("factorial", 
    Lambda("x", Variable("x")),
    Application(Variable "factorial", Constant 5)
  )
printf "%A" (evaluate Map.empty er')


// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
printf "%A" (evaluate Map.empty er)
