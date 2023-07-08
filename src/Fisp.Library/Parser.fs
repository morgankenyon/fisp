namespace Fisp.Library

module Parser = 
    open Lexer
    open Ast

    type ExprPrecedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    | INDEX = 8

    let PrecedenceMap =
        Map.empty
            // .Add(TokenType.EQ, ExprPrecedence.EQUALS)
            // .Add(TokenType.NOT_EQ, ExprPrecedence.EQUALS)
            // .Add(TokenType.LT, ExprPrecedence.LESSGREATER)
            // .Add(TokenType.GT, ExprPrecedence.LESSGREATER)
            .Add(TokenType.PLUS, ExprPrecedence.SUM)
            .Add(TokenType.MINUS, ExprPrecedence.SUM)
            .Add(TokenType.SLASH, ExprPrecedence.PRODUCT)
            .Add(TokenType.ASTERISK, ExprPrecedence.PRODUCT)
            .Add(TokenType.LPAREN, ExprPrecedence.CALL)
            // .Add(TokenType.LBRACKET, ExprPrecedence.INDEX)
    
    type prefixParse = ParserState -> AstExpr option
    and ParserState = 
        {
            lexer : Lexer.LexerState
            mutable curToken : Token
            mutable peekToken : Token
            mutable errors : ResizeArray<string> //TODO - maybe change to option type??
            prefixParseFns : System.Collections.Generic.Dictionary<TokenType, prefixParse>
        }

    let nextToken (p: ParserState) =
        p.curToken <- p.peekToken
        p.peekToken <- Lexer.nextToken p.lexer

    let curTokenIs (p: ParserState) (t: TokenType) =
        p.curToken.TokenType = t

    let peekTokenIs (p: ParserState) (t: TokenType) =
        p.peekToken.TokenType = t

    let peekError (p: ParserState) (t: TokenType) =
        let msg = sprintf "expected next token to be %s, got %s instead" (t.ToString()) (p.peekToken.TokenType.ToString())
        p.errors.Add(msg)

    let expectPeek (p: ParserState) (t: TokenType) =
        match peekTokenIs p t with
        | true -> 
            nextToken p 
            true
        | false -> 
            peekError p t
            false
    
    let getTokenPrecedence tokenType =
        match PrecedenceMap.ContainsKey tokenType with
        | true ->
            PrecedenceMap.[tokenType]
        | false -> 
            ExprPrecedence.LOWEST

    let peekPrecedence p =
        getTokenPrecedence p.peekToken.TokenType

    let curPrecedence p =
        getTokenPrecedence p.curToken.TokenType

    let notRParen (p: ParserState) = 
        not (peekTokenIs p TokenType.RPAREN)
    
    let notEOF (p: ParserState) =
        not (peekTokenIs p TokenType.EOF)
    
    let notEOFAndRParen (p: ParserState) =
        notRParen p && notEOF p

    let parseIntegerLiteral p =
        match System.Int32.TryParse p.curToken.Literal with
        | true, l ->
            Int32 { token = p.curToken; value = l}
            |> Some
        | _ ->
            let errorMsg = sprintf "could not parse %s as integer" p.curToken.Literal
            p.errors.Add(errorMsg)
            None

    let parseDoubleLiteral p =
        match System.Double.TryParse p.curToken.Literal with
        | true, l ->
            Double { token = p.curToken; value = l}
            |> Some
        | _ ->
            let errorMsg = sprintf "could not parse %s as double" p.curToken.Literal
            p.errors.Add(errorMsg)
            None
    
    let parseBooleanLiteral p =
        match p.curToken.Literal with
        | "#t" ->
            Boolean { token = p.curToken; value = true }
            |> Some
        | "#f" ->
            Boolean { token = p.curToken; value = false }
            |> Some
        | _ ->
            let errorMsg = sprintf "could not parse %s as boolean" p.curToken.Literal
            p.errors.Add(errorMsg)
            None
    
    let parseStringLiteral p =
        String { token = p.curToken; value = p.curToken.Literal }
        |> Some

    let parseExpression (p: ParserState) (precedence: ExprPrecedence) =
        match p.prefixParseFns.ContainsKey p.curToken.TokenType with
        | true ->
            let prefix = p.prefixParseFns.[p.curToken.TokenType]
            prefix p
        | false ->
            p.errors.Add(sprintf "no prefix parse function for %s found" p.curToken.Literal)
            None

    let parseGroupedExpression p =
        nextToken p

        match parseExpression p ExprPrecedence.LOWEST with
        | Some expression ->
            if not (expectPeek p TokenType.RPAREN) then None
            else Some expression
        | None -> None

    let parseBaseExpression (p: ParserState) =
        parseExpression p ExprPrecedence.LOWEST
    
    // let parsePrefixExpression (p: ParserState) : AstExpr option =
    //     let curToken = p.curToken

    //     nextToken p

    //     let firstPrecedence = curPrecedence p

    //     match parseExpression p firstPrecedence with
    //     | None ->
    //         None
    //     |  Some fv ->
    //         nextToken p

    //         let secondPrec = curPrecedence p

    //         match parseExpression p secondPrec with
    //         | None ->
    //             None
    //         | Some sv ->

    //             if peekTokenIs p TokenType.RPAREN || peekTokenIs p TokenType.EOF then
    //                 PrefixExpr { token = curToken; operator = curToken.Literal; firstValue = fv; secondValue = sv }
    //                 |> Some
    //             else
    //                 let errorMsg = "Error: + operator requires "
    let parsePrefixExpression (p: ParserState) : AstExpr option =
        let values = ResizeArray<AstExpr>()
        let curToken = p.curToken
        let op = curToken.Literal

        while notEOFAndRParen p do
            nextToken p
            let precedence = curPrecedence p
            
            match parseExpression p precedence with
            | Some v ->
                values.Add v
                ()
            | None ->
                ()
        
        if values.Count = 2 then
            PrefixExpr { token = curToken; operator = op; firstValue = values.[0]; secondValue = values.[1] }
            |> Some
        else
            let msg = sprintf "Error: %s operator requires 2 expressions, %d given." op values.Count
            p.errors.Add(msg)
            None

    let createParser lexer =
        let firstToken = Lexer.nextToken lexer
        let secondToken = Lexer.nextToken lexer

        let prefixFns = new System.Collections.Generic.Dictionary<TokenType, prefixParse>()
        prefixFns.Add(TokenType.PLUS, parsePrefixExpression)
        prefixFns.Add(TokenType.MINUS, parsePrefixExpression)
        prefixFns.Add(TokenType.ASTERISK, parsePrefixExpression)
        prefixFns.Add(TokenType.SLASH, parsePrefixExpression)
        prefixFns.Add(TokenType.INT, parseIntegerLiteral)
        prefixFns.Add(TokenType.DOUBLE, parseDoubleLiteral)
        // prefixFns.Add(TokenType.BANG, parsePrefixExpression)
        // prefixFns.Add(TokenType.MINUS, parsePrefixExpression)
        prefixFns.Add(TokenType.TRUE, parseBooleanLiteral)
        prefixFns.Add(TokenType.FALSE, parseBooleanLiteral)
        prefixFns.Add(TokenType.LPAREN, parseGroupedExpression)
        prefixFns.Add(TokenType.LT, parsePrefixExpression)
        prefixFns.Add(TokenType.GT, parsePrefixExpression)
        //prefixFns.Add(TokenType.LT, parsePrefixExpression)
        //prefixFns.Add(TokenType.GT, parsePrefixExpression)
        // prefixFns.Add(TokenType.IF, parseIfExpression)
        // prefixFns.Add(TokenType.FUNCTION, parseFunctionLiteral)
        prefixFns.Add(TokenType.STRING, parseStringLiteral)
        // prefixFns.Add(TokenType.LBRACKET, parseArrayLiteral)
        // prefixFns.Add(TokenType.LBRACE, parseHashLiteral)

        let parser = 
            { 
                lexer = lexer
                curToken = firstToken
                peekToken = secondToken
                errors = new ResizeArray<string>()
                prefixParseFns = prefixFns
            }

        parser

    let parseProgram (parser: ParserState) : Ast.AstExpr =
        let parsedExpressions = new ResizeArray<AstExpr>()

        while not (curTokenIs parser TokenType.EOF) do
            match parseBaseExpression parser with
            | Some expr -> 
                parsedExpressions.Add(expr)
            | None -> ()

            nextToken parser

        let exprArray = parsedExpressions.ToArray()

        Program { expressions = exprArray }

    let parse lexer =
        createParser lexer
        |> parseProgram