namespace Fisp.Library

module Ast =
    open Lexer
    type Integer32 = 
        {
            token: Token
            value: int32
        }
    type Double =
        {
            token: Token
            value: double
        }
    type Boolean =
        {
            token: Token
            value: bool
        }
    type Str =
        {
            token: Token
            value: string
        }
    and PrefixExpression = 
        {
            token: Token
            operator: string
            firstValue: AstExpr
            secondValue: AstExpr
        }
    and QExpression =
        {
            token: Token;
            expr: AstExpr
        }
    // and ExpressionStatement(token: Token, expression: AstExpr) =
    //     member this.token = token
    //     member this.expression = expression
    and Program =
        {
            expressions: AstExpr[]
        }
    and AstExpr =
    | Int32 of Integer32
    | PrefixExpr of PrefixExpression
    | Program of Program
    | Double of Double
    | Boolean of Boolean
    | String of Str