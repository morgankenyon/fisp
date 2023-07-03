
open System
open System.IO

[<EntryPoint>]
let main argv =
    printfn("Fispy Version 0.0.0.0.1");
    printfn("Press Ctrl+c to Exit\n");

    let prompt = "fispy> "
    while true do
        Console.Write(prompt)
        let line = Console.ReadLine()

        printfn "No you're a %s" line
    0