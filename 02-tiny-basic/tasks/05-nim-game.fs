// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
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
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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

let binaryNumOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
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
    | "-" -> binaryNumOp (-) lst
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
    | "MIN" -> binaryNumOp min lst
    | "RND" ->
      match lst with
      | [NumberValue f] -> NumberValue(state.Random.Next(f))
      | _ -> failwith ("unsupported arguments: " + name)
    | _ -> failwith ("unknown function: " + name)


let rec runCommand state (line, cmd) =
  match cmd with
  | Print(expr) ->
      expr |> List.iter (fun exp -> evalExpression state exp |> printValue)
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
  | Input name ->
    let (_, v) = Seq.initInfinite (fun _ -> System.Console.ReadLine() |> System.Int32.TryParse) |> Seq.find (fun (p, _) -> p)
    runNextLine {state with Variables = Map.add name (NumberValue v) state.Variables} line


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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
