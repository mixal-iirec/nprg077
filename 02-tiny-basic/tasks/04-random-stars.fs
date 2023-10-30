// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
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
  | Stop
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random
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

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  match expr with
  | Const c -> c
  | Variable v ->
    match state.Variables.TryFind v with 
    | Some res -> res
    | _ -> failwith ("unbound variable: " + v)
  | Function (name, lst) ->
    let lst = List.map (fun exp -> evalExpression state exp) lst
    match name with
    | "-" ->
      match lst with
      | [NumberValue f; NumberValue s] -> NumberValue(f - s)
      | _ -> failwith ("unsupported arguments: " + name)
    | "=" -> 
      match lst with
      | [f;s] -> BoolValue (f = s)
      | _ -> failwith ("unsupported number of arguments: " + name)
    | "||" ->
      match lst with
      | [BoolValue f; BoolValue s] -> BoolValue(f || s)
      | _ -> failwith ("unsupported arguments: " + name)
    | "<" -> binaryRelOp (<) lst
    | ">" -> binaryRelOp (>) lst
    | "RND" ->
      match lst with
      | [NumberValue f] -> NumberValue(state.Random.Next(f))
      | _ -> failwith ("unsupported arguments: " + name)
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
  | Clear ->
    System.Console.Clear ()
    runNextLine state line
  | Poke (x, y, s) ->
    let x = evalExpression state x
    let y = evalExpression state y
    let s = evalExpression state s
    match (x,y,s) with
    | (NumberValue x, NumberValue y, StringValue s) ->
      System.Console.SetCursorPosition(x, y)
      System.Console.Write(s)
    | _ -> failwith "unsupported arg"
    runNextLine state line


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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random.Shared }

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
