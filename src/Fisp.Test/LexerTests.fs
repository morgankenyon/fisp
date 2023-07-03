module Tests

open System
open Xunit
open Fisp.Library


let buildTokenTypes (tokens : Lexer.TokenType * string) : Lexer.Token list=
    tokens |> List.map (fun (t, l) -> { TokenType = t ; Literal = l })

let assertTokens expectedTokens actualTokens = 
    Assert.Equal(expectedTokens.Length, actualTokens.Length)
[<Fact>]
let ``Can lex simple add expression`` () =
    let input = "5 + 5"

    let lexedInput = Lexer.lexInput input

    Assert.Equal(4, lexedInput.Length)

    let expectedTokensRaw:(TokenType * string) list =
        [
            (TokenType.INT, "5");
            (TokenType.PLUS, "+");
            (TokenType.INT, "5");
            (TokenType.EOF, "");
        ]

    let expectedTokens = buildTokenTypes expectedTokensRaw



[<Fact>]
