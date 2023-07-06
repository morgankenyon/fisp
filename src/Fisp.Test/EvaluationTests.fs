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
[<InlineData("+ (+ 5 10) (+ 15 20))", 50)>]
[<InlineData("* 5 10", 50)>]
[<InlineData("(* 5 10)", 50)>]
[<InlineData("* (* 5 10) (* 15 20))", 15000)>]
[<InlineData("- 5 10", -5)>]
[<InlineData("(- 5 10)", -5)>]
[<InlineData("- (- 100 10) (- 15 20))", 95)>]
[<InlineData("/ 10 5", 2)>]
[<InlineData("(/ 10 5)", 2)>]
[<InlineData("/ (/ 100 5) (/ 30 15))", 10)>]
[<InlineData("(+ (/ 20 2) (* 5 5) (- 10 5))", 40)>]
let ``Can test integer formulas`` input expected =
    let result = evaluate input

    match result with
    | Objects.Int32Obj i32 ->
        Assert.Equal(expected, i32.value)
    //| _ -> Assert.True(false, "Wrong type returned from evaluate")