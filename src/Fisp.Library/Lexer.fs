namespace Fisp.Library

module Lexer =
    type TokenType =
        // special types
        | ILLEGAL
        | EOF
        //literals
        | INT
        | DOUBLE
        | TRUE
        | FALSE
        | STRING
        //operators
        | PLUS
        | MINUS
        | ASTERISK
        | SLASH
        | LT
        | GT
        //delimiters
        | LPAREN
        | RPAREN
        | LBRACE
        | RBRACE

    type ComplexTokenType =
        | Digit
        | Illegal
        | Hash
    
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

    let isCharEOF (l: LexerState) =
        l.ch = '\000'

    let isDigit(ch: char) =
        ch.CompareTo('0') >= 0 && ch.CompareTo('9') <= 0
    
    let isHash(ch: char) =
        ch = '#'

    let findComplexTokenType l =
        if isDigit(l.ch) then
            Digit
        else if isHash(l.ch) then
            Hash
        else
            Illegal

    let canReadDigit(l: LexerState) =
        //ensure I can read next position
        let canReadNextPosition = l.position + 1 < l.input.Length
        canReadNextPosition && isDigit(l.input.Chars(l.position + 1)) 
    
    let readString (l: LexerState) =
        let startPosition = l.position + 1
        
        readChar l
        while l.ch <> '"' && l.ch <> '\000' do
            readChar l

        l.input.Substring(startPosition, l.position - startPosition)

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
    
    let notHitWhitespace (l: LexerState) =
        l.ch <> ' '
        
    let readHash(l: LexerState) =
        let pos = l.position

        while notHitWhitespace(l) && not (isCharEOF l) do
            readChar l
        
        let literal = l.input.Substring(pos, (l.position - pos))

        match literal with
        | "#t" -> (TRUE, literal)
        | "#f" -> (FALSE, literal)
        | _ -> (ILLEGAL, literal)

    let nextComplexToken(l: LexerState) =
        match findComplexTokenType(l) with 
        | Digit -> readNumber(l)
        | Hash -> readHash(l)
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
            | '<' -> (TokenType.LT, l.ch.ToString())
            | '>' -> (TokenType.GT, l.ch.ToString())
            | '"' -> 
                let literal = readString l
                (TokenType.STRING, literal)
            | '{' -> (TokenType.LBRACE, l.ch.ToString())
            | '}' -> (TokenType.RBRACE, l.ch.ToString())
            // | '!' ->
            //     let nextChar = peekChar l
            //     match nextChar with
            //     | '=' ->
            //         let ch = l.ch
            //         readChar l
            //         (TokenType.NOT_EQ, ch.ToString() + l.ch.ToString())
            //     | _ -> (TokenType.BANG, l.ch.ToString())
            // | ',' -> (TokenType.COMMA, l.ch.ToString())
            // | ';' -> (TokenType.SEMICOLON, l.ch.ToString())
            // | '[' -> (TokenType.LBRACKET, l.ch.ToString())
            // | ']' -> (TokenType.RBRACKET, l.ch.ToString())
            // | ':' -> (TokenType.COLON, l.ch.ToString())
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