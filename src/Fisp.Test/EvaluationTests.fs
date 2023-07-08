module EvaluationTests

open Xunit
open Fisp.Library
open Fisp.Library.Lexer
open Fisp.Library.Parser
open Fisp.Library.Objs
open Fisp.Library.Evaluation

let evaluate input =
    let parser = 
        createLexer input
        |> createParser

    let program =
        parser
        |> parseProgram

    
    let errorMessage =  
        if parser.errors.Count > 0 then 
            let error = parser.errors.ToArray() |> Array.reduce (fun a b -> sprintf "%s\n%s" a b)
            error
        else ""
    
    Assert.True(0 = parser.errors.Count, errorMessage)
    
    evaluate program

let canAssertDoubleWithPrecision precision expected result =
    match result with
    | DoubleObj dbl ->
        Assert.Equal(expected, dbl.value, precision)
    | _ -> Assert.True(false, "Wrong type returned from evaluate")

let canAssertDouble expected result =
    canAssertDoubleWithPrecision 5 expected result

let canAssertBoolean expected result =
    match result with
    | BoolObj bol ->
        Assert.Equal(expected, bol.value)
    | _ -> Assert.True(false, "Wrong type returned from evaluate")

let canAssertErrorMsg expected result =
    match result with
    | ErrorObj err ->
        Assert.Equal(expected, err.msg)
    | _ -> Assert.True(false, "Expecting error from evaluate")

let canAssertStr expected result =
    match result with
    | StrObj str ->
        Assert.Equal(expected, str.value)
    | _ -> Assert.True(false, "Expecting Str, received another type")

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
[<InlineData("(+ (+ 5 10) (+ 15 20))", 50)>]
[<InlineData("* 5 10", 50)>]
[<InlineData("(* 5 10)", 50)>]
[<InlineData("(* (* 5 10) (* 15 20))", 15000)>]
[<InlineData("- 5 10", -5)>]
[<InlineData("(- 5 10)", -5)>]
[<InlineData("(- (- 100 10) (- 15 20))", 95)>]
[<InlineData("/ 10 5", 2)>]
[<InlineData("(/ 10 5)", 2)>]
[<InlineData("(/ (/ 100 5) (/ 30 15))", 10)>]
[<InlineData("(+ (/ 20 2) (* 5 5))", 35)>]
let ``Can test integer formulas`` input expected =
  let result = evaluate input

  match result with
  | Int32Obj i32 ->
      Assert.Equal(expected, i32.value)
  | _ -> Assert.True(false, "Wrong type returned from evaluate")

[<Theory>]
[<InlineData("+ 1 23.0")>]
[<InlineData("- 23.283 1")>]
[<InlineData("* 1 23.0")>]
[<InlineData("/ 23.0 2")>]
let ``Can assert mixed types returns errors`` input =
  let result = evaluate input

  canAssertErrorMsg "Bad operator and/or type for prefix expression" result

[<Theory>]
[<InlineData("#t", true)>]
[<InlineData("#f", false)>]
let ``Can test boolean literals`` input expected =
  let result = evaluate input

  canAssertBoolean expected result

[<Theory>]
[<InlineData("#t", "#t")>]
[<InlineData("#f", "#f")>]
let ``Can print out boolean literals correctly`` input expected =
  let result = evaluate input

  let objStr = Objs.printObj result

  Assert.Equal(expected, objStr)

[<Theory>]
[<InlineData("(< 2 4)", true)>]
[<InlineData("(< 4 2)", false)>]
[<InlineData("(> 4 2)", true)>]
[<InlineData("(> 2 4)", false)>]
let ``Can test simple boolean expressions`` input expected =
  let result = evaluate input

  canAssertBoolean expected result

[<Theory>]
[<InlineData("\"Hello world\"", "Hello world")>]
let ``Can evaluate a simple string`` input expected =
    let result = evaluate input

    canAssertStr expected result

[<Theory>]
[<InlineData("/ 10 0")>]
[<InlineData("/ 10.0 0.0")>]
let ``Will divide by zero throw exception`` input =
    let result = evaluate input

    canAssertErrorMsg "Error: Cannot divide by 0." result