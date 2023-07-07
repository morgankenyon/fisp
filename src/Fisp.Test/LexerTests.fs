module LexerTests

open Xunit
open Fisp.Library
open Fisp.Library.Lexer


let buildTokenTypes (tokens : (TokenType * string) list) : Token[] =
    tokens 
    |> List.map (fun (t, l) -> { TokenType = t ; Literal = l })
    |> List.toArray

let assertToken (expectedToken: Token) (actualToken: Token) =

    Assert.Equal(expectedToken.TokenType, actualToken.TokenType)
    Assert.Equal(expectedToken.Literal, actualToken.Literal)
    ()

let assertLexedTokens (expectedTokens: Token[]) (actualTokens: Token[]) = 
    let actualTokenMsg = 
        actualTokens
        |> Array.map (fun t -> t.TokenType.ToString())
        |> Array.reduce (fun a b -> sprintf "%s %s" a b)

    Assert.True(expectedTokens.Length = actualTokens.Length, actualTokenMsg)

    Array.zip expectedTokens actualTokens
    |> Array.iter (fun (et, at) -> assertToken et at)

let canAssertLexing input expectedTokensRaw =
    let lexedInput = lexInput input

    let expectedTokens = buildTokenTypes expectedTokensRaw

    assertLexedTokens expectedTokens lexedInput

[<Fact>]
let ``Can lex simple add expression`` () =
    let input = "+ 5 5"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.PLUS, "+");
            (TokenType.INT, "5");
            (TokenType.INT, "5");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex simple minus expression`` () =
    let input = "- 5 5"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.MINUS, "-");
            (TokenType.INT, "5");
            (TokenType.INT, "5");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex simple slash expression`` () =
    let input = "/ 5 5"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.SLASH, "/");
            (TokenType.INT, "5");
            (TokenType.INT, "5");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex simple asterisk expression`` () =
    let input = "* 5 5"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.ASTERISK, "*");
            (TokenType.INT, "5");
            (TokenType.INT, "5");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex complex expression`` () =
    let input = "/ 1000 (- 10 (+ 5 (* 5 5)))"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.SLASH, "/");
            (TokenType.INT, "1000");
            (TokenType.LPAREN, "(");
            (TokenType.MINUS, "-");
            (TokenType.INT, "10");
            (TokenType.LPAREN, "(");
            (TokenType.PLUS, "+");
            (TokenType.INT, "5");
            (TokenType.LPAREN, "(");
            (TokenType.ASTERISK, "*");
            (TokenType.INT, "5");
            (TokenType.INT, "5");
            (TokenType.RPAREN, ")");
            (TokenType.RPAREN, ")");
            (TokenType.RPAREN, ")");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex addition with doubles`` () =
    let input = "+ 5.0 5.0"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.PLUS, "+");
            (TokenType.DOUBLE, "5.0");
            (TokenType.DOUBLE, "5.0");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex multiple addition with doubles`` () =
    let input = "+ 1023.238 78.43 35.60007"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.PLUS, "+");
            (TokenType.DOUBLE, "1023.238");
            (TokenType.DOUBLE, "78.43");
            (TokenType.DOUBLE, "35.60007");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex true`` () =
    let input = "#t"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.TRUE, "#t");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex false`` () =
    let input = "#f"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.FALSE, "#f");
            (TokenType.EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can let less than`` () =
    let input = "(< 2 3)"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (LPAREN, "(");
            (LT, "<");
            (INT, "2");
            (INT, "3");
            (RPAREN, ")");
            (EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can let greater than`` () =
    let input = "(> 2 3)"

    let expectedTokensRaw:(TokenType * string) list =
        [
            (LPAREN, "(");
            (GT, ">");
            (INT, "2");
            (INT, "3");
            (RPAREN, ")");
            (EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex strings`` () =
    let input = "\"Hello world\""

    let expectedTokensRaw:(TokenType * string) list =
        [
            (STRING, "Hello world");
            (EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

[<Fact>]
let ``Can lex escaped string`` () =
    let input = "\"Hello \"world\"\""

    let expectedTokensRaw:(TokenType * string) list =
        [
            (STRING, "Hello \"world\"");
            (EOF, "");
        ]

    canAssertLexing input expectedTokensRaw

