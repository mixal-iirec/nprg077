// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Stop

type State = 
  { Program : list<int * Command>
    Variables : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  match value with
  | StringValue str -> System.Console.Write (str)
  | NumberValue num -> System.Console.Write (num)
  | BoolValue b -> System.Console.Write (b)

let getLine state line =
  List.tryFind (fun (l, _) -> l >= line) state.Program |> Option.defaultValue (-1, Stop)

let addLine state (line, cmd) = 
  {state with Program = (line, cmd) :: (List.filter (fun (l, _) -> l <> line) state.Program) |> List.sortBy (fst)}

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr = 
  match expr with
  | Const c -> c
  | Variable v ->
    match state.Variables.TryFind v with 
    | Some res -> res
    | _ -> failwith ("unbound variable: " + v)
  | Function (name, lst) ->
    match name with
    | "-" ->
      match lst with
      | f::s::[] ->
        let f = evalExpression state f
        let s = evalExpression state s
        match (f, s) with
        | (NumberValue f, NumberValue s) -> NumberValue (f - s)
        | _ -> failwith ("unsupported arguments: " + name)
      | _ -> failwith ("unsupported number of arguments: " + name)
    | "=" -> 
      match lst with
      | f::s::[] ->
        let f = evalExpression state f
        let s = evalExpression state s
        BoolValue (f = s)
      | _ -> failwith ("unsupported number of arguments: " + name)
    | _ -> failwith ("unknown function: " + name)

let rec runCommand state (line, cmd) =
  match cmd with
  | Print(expr) ->
      evalExpression state expr |> printValue
      runNextLine state line
  | Run -> getLine state 0 |> runCommand state
  | Goto(line) -> getLine state line |> runCommand state
  | Stop -> state
  | Assign(name, exp) -> runNextLine {state with Variables = Map.add name (evalExpression state exp) state.Variables} line
  | If (exp, cmd) ->
    match evalExpression state exp with
    | BoolValue true -> runCommand state (line, cmd)
    | BoolValue false -> runNextLine state line
    | _ -> failwith "only bools can be in if"
    

and runNextLine state line = getLine state (line+1) |> runCommand state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | Some line -> addLine state (line, cmd)
  | None -> runCommand state (System.Int32.MaxValue, cmd)

let runInputs state cmds = List.fold (fun state' cmd -> runInput state' cmd) state cmds

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; Variables = Map.empty }

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
