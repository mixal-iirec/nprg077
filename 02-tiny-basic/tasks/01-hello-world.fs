// ----------------------------------------------------------------------------
// 01 - Add GOTO and better PRINT for infinite loop fun!
// ----------------------------------------------------------------------------

// NOTE: You can run this using 'dotnet run' from the terminal. 
// If you want to run code in a different file, you will need to change
// the 'tinybasic.fsproj' file (which references this source file now).

// NOTE: F# code in projects is generally organized using namespaces and modules.
// Here, we declare module name for the source code in this file.
module TinyBASIC

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  // NOTE: GOTO specified line number. Note that this is an integer, rather 
  // than an expression, so you cannot calculate line number dynamically. 
  // (But there are tricks to do this by direct memory access on a real C64!)
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
// Test cases
// ----------------------------------------------------------------------------

let helloOnce =
  { Program = [
      10, Print (Const (StringValue "HELLO WORLD\n")) ] }

let helloInf =
  { Program = [
      10, Print (Const (StringValue "HELLO WORLD\n"))
      20, Goto 10 ] }

// NOTE: First try to get the following to work!
runCommand helloOnce (-1, Run) |> ignore

// NOTE: Then add 'Goto' and get the following to work!
runCommand helloInf (-1, Run) |> ignore

