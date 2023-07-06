module EvaluationTests

open Xunit
open Fisp.Library
open Fisp.Library.Lexer
open Fisp.Library.Parser
open Fisp.Library.Objs
open Fisp.Library.Evaluation

let evaluate input =
    let program = 
        createLexer input
        |> createParser
        |> parseProgram
    
    evaluate program

[<Theory>]
[<InlineData("5", 5)>]
let ``Can test integer values`` input expected =
    let result = evaluate input

    match result with
    | Objects.Int32Obj i32 ->
        Assert.Equal(expected, i32.value)
    //| _ -> Assert.True(false, "Wrong type returned from evaluate")

[<Theory>]
[<InlineData("+ 5 10", 15)>]
let ``Can test simple addition`` input expected =
    let result = evaluate input

    match result with
    | Objects.Int32Obj i32 ->
        Assert.Equal(expected, i32.value)
    //| _ -> Assert.True(false, "Wrong type returned from evaluate")