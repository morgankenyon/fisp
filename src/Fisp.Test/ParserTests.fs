module ParserTests

open Xunit
open Fisp.Library
open Fisp.Library.Lexer
open Fisp.Library.Parser

// let canDowncastToPrefixExpr (expr: Ast.AstExpr) =
//     match expr with 
//     | :? Object.Integer as int -> true
//     | _ -> false

let assertInt32 expected (expr: Ast.AstExpr) =
    match expr with
    | Ast.Int32 i32 ->
        Assert.Equal(expected, i32.value)
    | _ -> Assert.True(false, "Expecting Int32, got different type")

let assertDouble expected (expr: Ast.AstExpr) =
    match expr with
    | Ast.Double dbl ->
        Assert.Equal(expected, dbl.value)
    | _ -> Assert.True(false, "Expecting Int32, got different type")

let assertBoolean expected (expr: Ast.AstExpr) =
    match expr with
    | Ast.Boolean bol ->
        Assert.Equal(expected, bol.value)
    | _ -> Assert.True(false, "Expecting Boolean, got different type")

let assertString expected (expr: Ast.AstExpr) =
    match expr with
    | Ast.String str ->
        Assert.Equal(expected, str.value)
    | _ -> Assert.True(false, "Expecting String, got different type")

let isPrefixExpr (expr: Ast.AstExpr) =
    match expr with
    | Ast.PrefixExpr pe -> true
    | _ -> false

let assert2aryIntPrefixExpr (expr: Ast.AstExpr) operator firstNum secondNum =
    match expr with
    | Ast.PrefixExpr pe ->
        Assert.Equal(operator, pe.operator)

        let first = pe.firstValue
        let second = pe.secondValue

        assertInt32 firstNum first
        assertInt32 secondNum second
    | _ -> Assert.True(false, "Wrong expression type returned from test")

let assert2aryDoublePrefixExpr (expr: Ast.AstExpr) operator firstNum secondNum =
    match expr with
    | Ast.PrefixExpr pe ->
        Assert.Equal(operator, pe.operator)

        let first = pe.firstValue
        let second = pe.secondValue

        assertDouble firstNum first
        assertDouble secondNum second

    | _ -> Assert.True(false, "Wrong expression type returned from test")

let assertQExpression (expr: Ast.AstExpr) =
    match expr with
    | Ast.QExpression with
    | _ -> Assert.True(false, "Wrong expression type returned from test")

let canAssertBasicItems input =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    let errorMessage =  
        if parser.errors.Count > 0 then 
            let error = parser.errors.ToArray() |> Array.reduce (fun a b -> sprintf "%s\n%s" a b)
            error
        else ""

    Assert.True(parser.errors.Count = 0, errorMessage)

    match program with
    | Ast.AstExpr.Program p ->


        let expressionLength = p.expressions.Length

        Assert.True(1 = p.expressions.Length, sprintf "Was expecteding 1 expression, had %d" expressionLength)

        p.expressions.[0]
    | _ -> 
        Assert.True(false, "Returned a different type from parseProgram")
        program

let canAssertParserErrorsPresent input =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    Assert.False(0 = parser.errors.Count, "Was expecting errors from expressions, but none present")

let canAssertParserError input expected =
    let lexer = createLexer input
    let parser = createParser lexer
    let program = parseProgram parser

    let msg = sprintf "Was expecting 1 parser error, got %d" parser.errors.Count
    Assert.True(1 = parser.errors.Count, msg)

    let error = parser.errors.[0]

    Assert.Equal(expected, error)


[<Theory>]
[<InlineData("+ 5 10", "+", 5, 10)>]
[<InlineData("- 5 10", "-", 5, 10)>]
[<InlineData("* 5 10", "*", 5, 10)>]
[<InlineData("/ 5 10", "/", 5, 10)>]
let ``Can parse simple arithmetic expression`` input op firstNum secondNum =
    let expression = canAssertBasicItems input

    assert2aryIntPrefixExpr expression op firstNum secondNum

// [<Fact>]
// let ``Can parse more complex plus expression`` () =
//     let input = "+ 5 10 15 20"

//     let expression = canAssertBasicItems input

//     match expression with
//     | Ast.PrefixExpr pe ->
//         Assert.Equal("+", pe.operator)
//         Assert.Equal(4, pe.values.Length)

//         let five = pe.values.[0]
//         let ten = pe.values.[1]
//         let fifteen = pe.values.[2]
//         let twenty = pe.values.[3]

//         assertInt32 5 five
//         assertInt32 10 ten
//         assertInt32 15 fifteen
//         assertInt32 20 twenty
//     | _ -> Assert.True(false, "Wrong expression type returned from test")
[<Theory>]
[<InlineData("+ 1 2 3", "Error: + operator requires 2 expressions, 3 given.")>]
let ``Can 3 or more expressions generate error`` input expected =
    canAssertParserError input expected

[<Fact>]
let ``Can handle parenthesis in expression`` () =
    let input = "(+ 5 10)"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)
    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Fact>]
let ``Can handle grouped expression`` () =
    let input = "(+ 5 (+ 10 15))"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)

        
        let five = pe.firstValue
        let subgroup = pe.secondValue

        assertInt32 5 five
        Assert.True(isPrefixExpr subgroup)
    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Fact>]
let ``Can handle multi grouped expression`` () =
    let input = "(+ (+ 5 10) (+ 15 20))"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)
        
        let firstSubGroup = pe.firstValue
        let secondSubGroup = pe.secondValue

        Assert.True(isPrefixExpr firstSubGroup)
        Assert.True(isPrefixExpr secondSubGroup)
    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Fact>]
let ``Can handle multi type grouped expression`` () =
    let input = "(- (* 5 10) (/ 15 20))"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("-", pe.operator)

        
        let firstSubGroup = pe.firstValue
        let secondSubGroup = pe.secondValue

        assert2aryIntPrefixExpr firstSubGroup "*" 5 10
        assert2aryIntPrefixExpr secondSubGroup "/" 15 20

    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Theory>]
[<InlineData("+ 23.78 33.58", "+", 23.78, 33.58)>]
[<InlineData("- 88.14 67.32", "-", 88.14, 67.32)>]
[<InlineData("* 202.11 600.22", "*", 202.11, 600.22)>]
[<InlineData("/ 2344.288 9299.22", "/", 2344.288, 9299.22)>]
let ``Can parse simple double expression`` input op firstNum secondNum =
    let expression = canAssertBasicItems input

    assert2aryDoublePrefixExpr expression op firstNum secondNum

[<Theory>]
[<InlineData("#t", true)>]
[<InlineData("#f", false)>]
let ``Can parse boolean literals`` input expected =
    let expression = canAssertBasicItems input

    assertBoolean expected expression

[<Theory>]
[<InlineData("< 2 3", "<", 2, 3)>]
[<InlineData("> 2 3", ">", 2, 3)>]
let ``Can parse comparison operators`` input op first second =
    let expr = canAssertBasicItems input

    assert2aryIntPrefixExpr expr op first second

[<Theory>]
[<InlineData("< 2 3 5")>]
[<InlineData("> 2 3 5")>]
let ``Can ensure comparison operators only have 2 expressions`` input =
    canAssertParserErrorsPresent input

[<Fact>]
let ``Can parse string`` () =
    let input = "\"Hello world\""

    let expr = canAssertBasicItems input

    assertString "Hello world" expr

//[<Fact>] //probably should be an error
let ``Can bad expression give error`` () =
    let input = "(+ 1 \"Hello world\")"

    canAssertParserErrorsPresent input

let ``Can parse q-expression`` () =
    let input = "{1 2 3 4}"

    let expr = canAssertBasicItems input

    assertQExpression