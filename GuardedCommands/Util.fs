// Michael R. Hansen 05-01-2016, 04-01-2018

namespace GuardedCommands.Util


open System.IO
open System.Text

open GuardedCommands.Frontend.AST
open Parser
open Lexer

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

    let parseString (text: string) =
        let lexbuf = LexBuffer<_>.FromString text
        try
            Main Lexer.tokenize lexbuf
        with e ->
            let pos = lexbuf.EndPos
            printfn "Error near line %d, character %d\n" pos.Line pos.Column
            failwith "parser termination"


    // Parse a file. (A statement is parsed)
    let parseFromFile filename =
        let path = "Examples/" + filename
        if File.Exists(path) then parseString (File.ReadAllText(path))
        else invalidArg "ParserUtil" "File not found"

open ParserUtil

open GuardedCommands.Backend
open GuardedCommands.Frontend.TypeCheck

module CompilerUtil =

    /// goOpt p compiles (using the optimized version) and runs an abstract syntax for a program
    let goOpt p = run (code2ints (CodeGenerationOpt.CP p))

    /// go p compiles and runs an abstract syntax for a program
    let go p = run (code2ints (CodeGeneration.CP p))

    /// goOpt p compile and runs an abstract syntax for a program showing a program trace
    let goTrace p = VirtualMachine.runTrace (code2ints (CodeGeneration.CP p))

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


