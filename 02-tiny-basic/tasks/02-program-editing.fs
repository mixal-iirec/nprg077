// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  | Stop

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  match value with
  | StringValue str -> printf "%s" str 

let getLine state line =
  List.tryFind (fun (l, _) -> l >= line) state.Program |> Option.defaultValue (-1, Stop)

let addLine state (line, cmd) = 
  {Program = (line, cmd) :: (List.filter (fun (l, _) -> l <> line) state.Program) |> List.sortBy (fst)}

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr =
  match expr with
  | Const v -> v


let rec runCommand state (line, cmd) =
  match cmd with
  | Print(expr) ->
      evalExpression expr |> printValue
      runNextLine state line
  | Run -> getLine state 0 |> runCommand state
  | Goto(line) -> getLine state line |> runCommand state
  | Stop -> state

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

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
