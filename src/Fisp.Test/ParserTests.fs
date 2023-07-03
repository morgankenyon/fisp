module Tests

open System
open Xunit
open Fisp.Library

[<Fact>]
let ``Can lex simple expression`` () =
    let input = "5 + 5"

    let lexedInput = Lexer.lexInput input

    Assert.Equal(4, lexedInput.Length)
