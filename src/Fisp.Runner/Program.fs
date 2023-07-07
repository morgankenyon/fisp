
open System
open System.IO
open Fisp.Library
let evaluateInput input =
    
    let parser = 
        Lexer.createLexer input
        |> Parser.createParser

    let program =
        Parser.parseProgram parser

    if parser.errors.Capacity > 0 then
        printfn "Errors present after parsing"
        for e in parser.errors do
            printfn "   %s" e
    else
    
        let evals = Evaluation.evaluate program

        let str = Objs.printObj evals

        printfn "%s" str

[<EntryPoint>]
let main argv =
    printfn("Fispy Version 0.0.1");
    printfn("Press Ctrl+c to Exit\n");

    let prompt = "fispy> "
    while true do
        Console.Write(prompt)
        let line = Console.ReadLine()

        evaluateInput line
    0