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

let isPrefixExpr (expr: Ast.AstExpr) =
    match expr with
    | Ast.PrefixExpr pe -> true
    | _ -> false

let assert2aryIntPrefixExpr (expr: Ast.AstExpr) operator firstNum secondNum =
    match expr with
    | Ast.PrefixExpr pe ->
        Assert.Equal(operator, pe.operator)
        Assert.Equal(2, pe.values.Length)

        let first = pe.values.[0]
        let second = pe.values.[1]

        assertInt32 firstNum first
        assertInt32 secondNum second

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

[<Theory>]
[<InlineData("+ 5 10", "+", 5, 10)>]
[<InlineData("- 5 10", "-", 5, 10)>]
[<InlineData("* 5 10", "*", 5, 10)>]
[<InlineData("/ 5 10", "/", 5, 10)>]
let ``Can parse simple plus expression`` input op firstNum secondNum =
    let expression = canAssertBasicItems input

    assert2aryIntPrefixExpr expression op firstNum secondNum

    // match expression with
    // | Ast.PrefixExpr pe ->
    //     Assert.Equal(op, pe.operator)
    //     Assert.Equal(2, pe.values.Length)

    //     let first = pe.values.[0]
    //     let second = pe.values.[1]

    //     assertInt32 firstNum first
    //     assertInt32 secondNum second
    // | _ -> Assert.True(false, "Wrong expression type returned from test")
    
[<Fact>]
let ``Can parse more complex plus expression`` () =
    let input = "+ 5 10 15 20"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)
        Assert.Equal(4, pe.values.Length)

        let five = pe.values.[0]
        let ten = pe.values.[1]
        let fifteen = pe.values.[2]
        let twenty = pe.values.[3]

        assertInt32 5 five
        assertInt32 10 ten
        assertInt32 15 fifteen
        assertInt32 20 twenty
    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Fact>]
let ``Can handle parenthesis in expression`` () =
    let input = "(+ 5 10)"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)
        Assert.Equal(2, pe.values.Length)
    | _ -> Assert.True(false, "Wrong expression type returned from test")

[<Fact>]
let ``Can handle grouped expression`` () =
    let input = "(+ 5 (+ 10 15))"

    let expression = canAssertBasicItems input

    match expression with
    | Ast.PrefixExpr pe ->
        Assert.Equal("+", pe.operator)
        Assert.Equal(2, pe.values.Length)

        
        let five = pe.values.[0]
        let subgroup = pe.values.[1]

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
        Assert.Equal(2, pe.values.Length)

        
        let firstSubGroup = pe.values.[0]
        let secondSubGroup = pe.values.[1]

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
        Assert.Equal(2, pe.values.Length)

        
        let firstSubGroup = pe.values.[0]
        let secondSubGroup = pe.values.[1]

        assert2aryIntPrefixExpr firstSubGroup "*" 5 10
        assert2aryIntPrefixExpr secondSubGroup "/" 15 20

    | _ -> Assert.True(false, "Wrong expression type returned from test")
