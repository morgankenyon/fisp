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

let canAssertDoubleWithPrecision precision expected result =
    match result with
    | DoubleObj dbl ->
        Assert.Equal(expected, dbl.value, precision)
    | _ -> Assert.True(false, "Wrong type returned from evaluate")

let canAssertDouble expected result =
    canAssertDoubleWithPrecision 5 expected result

[<Theory>]
[<InlineData("5", 5)>]
let ``Can test integer values`` input expected =
    let result = evaluate input

    match result with
    | Int32Obj i32 ->
        Assert.Equal(expected, i32.value)
    | _ -> Assert.True(false, "Wrong type returned from evaluate")

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
    | Int32Obj i32 ->
        Assert.Equal(expected, i32.value)
    | _ -> Assert.True(false, "Wrong type returned from evaluate")

//[<Theory>]
//[<InlineData("+ 23.78 33.58", 57.36)>]
//[<InlineData("- 88.14 67.32", 20.82)>]
//[<InlineData("* 202.11 600.22", 121310.4642)>]
//[<InlineData("/ 2344.288 22.33", 104.983788625)>]
//let ``Can test double formulas`` input expected =
//    let result = evaluate input

//    match result with
//    | DoubleObj dbl ->
//        Assert.Equal(expected, dbl.value, 5)
//    | _ -> Assert.True(false, "Wrong type returned from evaluate")

[<Theory>]
[<InlineData("+ 1 23.0", 24.0)>]
[<InlineData("- 23.283 1", 22.283)>]
[<InlineData("* 1 23.0", 23.0)>]
[<InlineData("/ 23.0 2", 11.5)>]
let ``Can test arithmetic for mixed expressions`` input expected =
    let result = evaluate input

    canAssertDouble expected result

