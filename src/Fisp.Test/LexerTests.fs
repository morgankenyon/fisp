module Tests

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
    Assert.Equal(expectedTokens.Length, actualTokens.Length)

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