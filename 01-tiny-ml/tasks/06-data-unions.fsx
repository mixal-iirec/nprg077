// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
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
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

and VariableContext = 
  Map<string, Value>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
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
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with 
      | ValNum n -> 
        match op with 
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
      | _ -> failwith "unary operator unsupported on closures"
  | If (e1, e2, e3) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      let v3 = evaluate ctx e3
      match v1 with 
      | ValNum n1 -> 
            if n1 = 1 then v2 else v3
      | _ -> failwith "bool evaluation unsupported on closures"
  
  | Lambda(v, e) -> ValClosure (v, e, ctx)

  | Application(e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2

      match v1 with 
      | ValClosure (v, e, ctx) -> 
          let ctx = Map.add v v2 ctx
          evaluate ctx e
      | _ -> failwith "unsupported closure application"

  | Let(v, e1, e2) ->
      evaluate ctx (Application(Lambda(v, e2), e1))

  | Tuple(e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      ValTuple (v1, v2)
  | TupleGet(b, e) ->
      let v = evaluate ctx e
      match v with
        | ValTuple (c1, c2) ->
          if b then c1 else c2
        | _ -> failwith "tuple access only supported on tuple"

  | Match(e, v, e1, e2) ->
      let v0 = evaluate ctx e
      match v0 with
        | ValCase (b, v0) ->
          let ctx = Map.add v v0 ctx
          let e = if b then e1 else e2
          evaluate ctx e
        | _ -> failwith "matching without case unsupported"
      //let v1 = evaluate ctx e1
      //let v2 = evaluate ctx e2
      // TODO: Implement pattern matching. Note you need to
      // assign the right value to the variable of name 'v'!

  | Case(b, e) ->
      let v = evaluate ctx e
      ValCase (b, v)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
printf "%A" (evaluate Map.empty ec1)

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
printf "%A" (evaluate Map.empty ec2)

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
printf "%A" (evaluate Map.empty ec3)
