
open System
open System.IO
open Fisp.Library
let evaluateInput input =
    
    let program = 
        Lexer.createLexer input
        |> Parser.createParser
        |> Parser.parseProgram
    
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