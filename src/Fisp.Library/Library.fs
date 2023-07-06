namespace Fisp.Library

module Lexer =
    type TokenType =
        // special types
        | ILLEGAL
        | EOF
        //literals
        | INT
        | DOUBLE
        //operators
        | PLUS
        | MINUS
        | ASTERISK
        | SLASH
        //delimiters
        | LPAREN
        | RPAREN

    type ComplexTokenType =
        | Digit
        | Illegal
    
    type Token =
        {
            TokenType : TokenType
            Literal : string
        }
    
    type LexerState =
        {
            input : string
            mutable position : int
            mutable readPosition : int
            mutable ch : char
        }

    let readChar (l: LexerState) =
        let newChar =
            match l.readPosition >= l.input.Length with
            | true -> '\000'
            | false -> l.input.Chars l.readPosition
        l.position <- l.readPosition
        l.readPosition <- l.readPosition + 1
        l.ch <- newChar
    
    let peekNextCharIs (l: LexerState) testChar =
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && l.input.Chars(l.position + 1) = testChar

    let isDigit(ch: char) =
        ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0

    let findComplexTokenType l =
        if isDigit(l.ch) then
            Digit
        else
            Illegal

    let canReadDigit(l: LexerState) =
        //ensure I can read next position
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && isDigit(l.input.Chars(l.position + 1)) 

    let readNumber(l: LexerState) =
        let pos = l.position
        while canReadDigit(l) do 
            readChar l

        if peekNextCharIs l '.' then
            readChar l
            while canReadDigit(l) do
                readChar l
            let double = l.input.Substring(pos, (l.position - pos + 1))
            (DOUBLE, double)
        else   
            let literal = l.input.Substring(pos, (l.position - pos + 1))
            (INT, literal)

    let nextComplexToken(l: LexerState) =
        match findComplexTokenType(l) with 
        | Digit -> readNumber(l)
        | Illegal -> (TokenType.ILLEGAL, l.ch.ToString())

    let skipWhitespace(l: LexerState) =
        while l.ch = ' ' || l.ch = '\t' || l.ch = '\n' || l.ch = '\r' do
            readChar l
        ()
    
    let nextToken (l: LexerState) =

        skipWhitespace l

        let (tokenType, literal) =
            match l.ch with
            | '+' -> (TokenType.PLUS, l.ch.ToString())
            | '-' -> (TokenType.MINUS, l.ch.ToString())
            | '*' -> (TokenType.ASTERISK, l.ch.ToString())
            | '/' -> (TokenType.SLASH, l.ch.ToString())
            | '(' -> (TokenType.LPAREN, l.ch.ToString())
            | ')' -> (TokenType.RPAREN, l.ch.ToString())
            // | '!' ->
            //     let nextChar = peekChar l
            //     match nextChar with
            //     | '=' ->
            //         let ch = l.ch
            //         readChar l
            //         (TokenType.NOT_EQ, ch.ToString() + l.ch.ToString())
            //     | _ -> (TokenType.BANG, l.ch.ToString())
            // | '<' -> (TokenType.LT, l.ch.ToString())
            // | '>' -> (TokenType.GT, l.ch.ToString())
            // | ',' -> (TokenType.COMMA, l.ch.ToString())
            // | ';' -> (TokenType.SEMICOLON, l.ch.ToString())
            // | '{' -> (TokenType.LBRACE, l.ch.ToString())
            // | '}' -> (TokenType.RBRACE, l.ch.ToString())
            // | '[' -> (TokenType.LBRACKET, l.ch.ToString())
            // | ']' -> (TokenType.RBRACKET, l.ch.ToString())
            // | ':' -> (TokenType.COLON, l.ch.ToString())
            // | '"' -> 
            //     let literal = readString l
            //     (TokenType.STRING, literal)
            | '\000' -> (TokenType.EOF, "")
            | _ -> nextComplexToken l

        let token = { TokenType = tokenType; Literal = literal }
        
        readChar l
        token
    
    let createLexer input =
        let lexer = { input = input; position = 0; readPosition = 0; ch = '\000'}
        readChar lexer
        lexer

    let lexInput (input: string) : Token[] =
        let lexer = createLexer input
        let tokens = ResizeArray<Token>()
        let mutable keepLexing = true

        while keepLexing do
            let newToken = nextToken lexer
            tokens.Add(newToken)
            keepLexing <- newToken.TokenType <> TokenType.EOF

        tokens.ToArray()

module Ast =
    open Lexer
    type Integer32 = 
        {
            token: Token;
            value: int32
        }
    type Double =
        {
            token: Token;
            value: double
        }
    and PrefixExpression = 
        {
            token: Token;
            operator: string;
            values: AstExpr[]
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
    
    let parsePrefixExpression (p: ParserState) : AstExpr option =
        let values = ResizeArray<AstExpr>()
        let curToken = p.curToken

        while notEOFAndRParen p do
            nextToken p
            let precedence = curPrecedence p
            
            match parseExpression p precedence with
            | Some v ->
                values.Add v
                ()
            | None ->
                ()

        PrefixExpr { token = curToken; operator = curToken.Literal; values = values.ToArray() }
        |> Some
            
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
        // prefixFns.Add(TokenType.TRUE, parseBoolean)
        // prefixFns.Add(TokenType.FALSE, parseBoolean)
        prefixFns.Add(TokenType.LPAREN, parseGroupedExpression)
        // prefixFns.Add(TokenType.IF, parseIfExpression)
        // prefixFns.Add(TokenType.FUNCTION, parseFunctionLiteral)
        // prefixFns.Add(TokenType.STRING, parseStringLiteral)
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

module Objs =
    type Integer32 =
        {
            value: int32
        }
    type Double =
        {
            value: double
        }
    and Objects =
    | Int32Obj of Integer32
    | DoubleObj of Double

    let printObj (obj: Objects) =
        match obj with
        | Int32Obj int ->
            sprintf "%d" int.value
        | DoubleObj dbl ->
            sprintf "%f" dbl.value

module Evaluation =
    open Objs

    let evalIntegerPrefixes operator results =
        let mutable running = 
            match operator with
            | "+" -> 0
            | "-" -> 0
            | "*" -> 1
            | "/" -> 1
            | _ -> 0 //return error instead of 0 since we don't want bad operators

        let mutable isFirstLoop = true
        for re in results do 
            match operator, re with 
            | "+", Int32Obj num ->
                running <- running + num.value
                ()
            | "*", Int32Obj num ->
                running <- running * num.value
            | "-", Int32Obj num ->
                if isFirstLoop then
                    running <- num.value
                else
                    running <- running - num.value
            | "/", Int32Obj num ->
                if isFirstLoop then
                    running <- num.value
                else
                    running <- running / num.value
            | _ -> ()

            isFirstLoop <- false //hacky mechanism
        
        Int32Obj { value = running }

    let evalDoublePrefixes op results =
        let mutable running = 
            match op with
            | "+" -> 0.0
            | "-" -> 0.0
            | "*" -> 1.0
            | "/" -> 1.0
            | _ -> 0.0 //return error instead of 0 since we don't want bad operators

        let firstMinusArg isFirst run num  =
            if isFirst then
                num
            else
                run - num

        let firstDivideArg isFirst run num =
            if isFirst then
                num
            else
                run / num

        let mutable isFirstLoop = true
        for re in results do 
            match op, re with 
            | "+", DoubleObj num ->
                running <- running + num.value
            | "+", Int32Obj num ->
                running <- running + (double)num.value
            | "*", DoubleObj num ->
                running <- running * num.value
            | "*", Int32Obj num ->
                running <- running * (double)num.value
            | "-", DoubleObj num -> 
                running <- firstMinusArg isFirstLoop running num.value
            | "-", Int32Obj num -> 
                let dblNum = (double) num.value
                running <- firstMinusArg isFirstLoop running dblNum
            | "/", DoubleObj num -> 
                running <- firstDivideArg isFirstLoop running num.value
            | "/", Int32Obj num -> 
                let dblNum = (double) num.value
                running <- firstDivideArg isFirstLoop running dblNum
            | _ -> ()

            isFirstLoop <- false //hacky mechanism
        
        DoubleObj { value = running }

    let rec eval (expr: Ast.AstExpr) =
        match expr with
        | Ast.Int32 i32 ->
            Int32Obj { value = i32.value }
        | Ast.Double dbl ->
            DoubleObj { value = dbl.value }
        | Ast.PrefixExpr pe ->
            evalPrefixExpr pe
        | Ast.Program program ->
            evalProgram program
    
    and evalPrefixExpr (pe: Ast.PrefixExpression) =
        let results = new ResizeArray<Objs.Objects>()
        
        let mutable hasDouble = false
        for v in pe.values do
            let result = eval v

            match result with
            | DoubleObj dbl ->
                hasDouble <- true
                ()
            | _ -> ()

            results.Add(result)
            ()

        if hasDouble then
            evalDoublePrefixes pe.operator results
        else
            evalIntegerPrefixes pe.operator results
        //determine whether list has doubles or ints
        
        // let mutable running = 
        //     match operator with
        //     | "+" -> 0
        //     | "-" -> 0
        //     | "*" -> 1
        //     | "/" -> 1
        //     | _ -> 0 //return error instead of 0 since we don't want bad operators

        // let mutable isFirstLoop = true
        // for re in results do 
        //     match operator, re with 
        //     | "+", Int32Obj num ->
        //         running <- running + num.value
        //         ()
        //     | "*", Int32Obj num ->
        //         running <- running * num.value
        //     | "-", Int32Obj num ->
        //         if isFirstLoop then
        //             running <- num.value
        //         else
        //             running <- running - num.value
        //     | "/", Int32Obj num ->
        //         if isFirstLoop then
        //             running <- num.value
        //         else
        //             running <- running / num.value
        //     | _ -> ()

        //     isFirstLoop <- false //hacky mechanism
        
        // Int32Obj { value = running }
    
    and evalProgram program =
        //this is junky, find a better way to only return the last one
        let results = new ResizeArray<Objs.Objects>()

        //if error, add early return step, check monkey
        for expr in program.expressions do 
            results.Add(eval expr)
            ()
        
        results.[results.Count - 1]



    let evaluate (expr: Ast.AstExpr) =
        //will add environment at some point
        eval expr
