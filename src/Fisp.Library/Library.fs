namespace Fisp.Library

module Lexer =
    type TokenType =
        // special types
        | ILLEGAL
        | EOF
        //literals
        | INT
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

module Parser = 
    open Lexer
    type ParserState = 
        {
            lexer : Lexer.LexerState
            mutable curToken : Token
            mutable peekToken : Token
            mutable errors : ResizeArray<string> //TODO - maybe change to option type??
            // prefixParseFns : System.Collections.Generic.Dictionary<TokenType, prefixParse>
            // infixParseFns : System.Collections.Generic.Dictionary<TokenType, infixParse>
        }

    