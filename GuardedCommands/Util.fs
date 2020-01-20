// Michael R. Hansen 05-01-2016, 04-01-2018

namespace GuardedCommands.Util


open System.IO
open System.Text

open GuardedCommands.Frontend.AST
open Parser
open Lexer

open System
open FSharp.Text.Lexing
open Machine
open VirtualMachine

//open GuardedCommands
//open GuardedCommands
//open GuardedCommands.Backend
//open Backend.CodeGeneration
//open Backend.CodeGenerationOpt
//open Backend.CodeGeneration
//open GuardedCommands.Frontend
//open GuardedCommands.Frontend.TypeCheck




module ParserUtil =

    type OutStr(sb:StringBuilder, orig:TextWriter) =
        inherit TextWriter()
        override x.Encoding = stdout.Encoding
        override x.Write (s:string) = sb.Append s |> ignore;
        override x.WriteLine (s:string) = sb.AppendLine s |> ignore;
        override x.WriteLine() = sb.AppendLine() |> ignore;
        member x.Value with get() = sb.ToString()
        static member Create() =
            let orig = stdout
            let out = new OutStr(StringBuilder(), orig)
            Console.SetOut(out)
            out
        member x.Reset () = Console.SetOut(orig)
        interface IDisposable with member x.Dispose() = Console.SetOut(orig)
    
    let parseString (text: string) =
        let lexbuf = LexBuffer<_>.FromString text
        try
            Main Lexer.tokenize lexbuf
        with e ->
            let pos = lexbuf.EndPos
            printfn "Error near line %d, character %d\n" pos.Line pos.Column
            failwith "parser termination"

    // Parse a file. (A statement is parsed)
    let parseFromFileBase filename =
        if File.Exists(filename) then parseString (File.ReadAllText(filename))
        else invalidArg "ParserUtil" "File not found"

    let parseFromFile filename =
        parseFromFileBase ("Examples/" + filename)

    

open ParserUtil

open System
open GuardedCommands.Backend
open GuardedCommands.Frontend.TypeCheck

module CompilerUtil =

    /// goOpt p compiles (using the optimized version) and runs an abstract syntax for a program
    let goOpt p = run (code2ints (CodeGenerationOpt.CP p))

    /// go p compiles and runs an abstract syntax for a program
    let go p = run (code2ints (CodeGeneration.CP p))

    /// goOpt p compile and runs an abstract syntax for a program showing a program trace
    let goTrace p = VirtualMachine.runTrace (code2ints (CodeGeneration.CP p))

    let goTraceOpt p = VirtualMachine.runTrace (code2ints (CodeGenerationOpt.CP p))

    /// exec filename parses, type checks, compiles and runs a program in a file
    let exec filename =
        printfn "\nParse, typecheck, compilation and execution of %s:" filename
        let prog = parseFromFile filename
        tcP prog
        go prog

    /// execOpt filename parses, type checks, compiles and runs a program in a file
    let execOpt filename =
        printfn "\nParse, typecheck, optimized compilation and execution of %s:" filename
        let prog = parseFromFile filename
        tcP prog
        goOpt prog

    /// execTrace filename parses, type checks, compiles and runs a program in a file showing a program trace
    let execTrace filename =
        printfn "\nParse, typecheck, compilation and execution of %s:" filename
        let prog = parseFromFile filename
        tcP prog
        
        goTrace prog

    let execOptTest (files): int =
        printfn "Comparing results from CodeGen and CodeGenOpt"
        let rec inner (files: string list) (res: string list):string list = match files with
            | [] -> res
            | x :: xs ->
                let prog = parseFromFileBase x
                tcP prog
                
                let out = OutStr.Create()
                go prog |> ignore
                let slowStack = Array.head (out.Value.Split "Stack")
                out.Reset() |> ignore
                
                let out = OutStr.Create()
                go prog |> ignore
                let optStack = Array.head (out.Value.Split "Stack")
                out.Reset() |> ignore
                
                match slowStack = optStack with
                | true -> inner xs res
                | false ->
                    inner xs ((sprintf "%A failed" x) ::
                    (sprintf "%A" slowStack) ::
                    (sprintf "%A" optStack) :: res)
        let rec print (xs:string list): int = match xs with
            | x :: xs ->
                if x.Length > 0 then printfn "%s" x
                print xs
            | _ -> 0
        inner files [] |> print
    
    /// execTest parses, type checks, compiles and runs every program in the Examples folder
    let execTest (go: Program -> unit) =
        printfn "Testing all files..."

        let testFiles = Directory.GetFiles("Examples", "*.gc", SearchOption.AllDirectories)

        let rec testing (files: string list) (failed: (string * exn) list) =
            match files with
            | [] -> failed
            | x :: t -> 
                try
                    let prog = parseFromFileBase x
                    tcP prog
                    goOpt prog
                    testing t failed
                with
                    | ex -> testing t ((x, ex) :: failed)

        let failed = testing (Array.toList testFiles) List.empty
        printfn "Failed:"
        printfn "%A" failed