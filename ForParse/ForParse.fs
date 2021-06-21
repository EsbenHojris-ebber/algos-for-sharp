(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type Position = {
        line    : int
        coloumn : int
    }

    let initialPos = { line = 0; coloumn = 0 }

    let incrCol pos = { pos with coloumn = pos.coloumn + 1 }

    let incrLine pos = { line = pos.line; coloumn = 0 }

    type InputState = {
        lines    : string[]
        position : Position
    }

    module InputState =
        open System

        let fromString str =
            if String.IsNullOrEmpty str then
                { lines = [||]; position = initialPos }
            else
                let sep = [| "\r\n"; "\n" |]
                let lines = str.Split(sep, StringSplitOptions.None)
                { lines = lines; position = initialPos}

        let currentLine inputState =
            let linepos = inputState.position.line
            if linepos < inputState.lines.Length then
                inputState.lines.[linepos]
            else
                "end of file"

        let nextChar input =
            let linePos = input.position.line
            let colPos = input.position.coloumn

            if linePos >= input.lines.Length then
                input, None
            else
                let currentLine = currentLine input
                if colPos < currentLine.Length then
                    { input with position = incrCol input.position }, currentLine.[colPos] |> Some
                else
                    { input with position = incrLine input.position }, Some '\n'
    
    type ParserLabel = string
    type ParserError = string
    type ParserPosition = {
        currentLine : string
        line    : int
        coloumn : int
    }

    let parserPositionFromInputState input = {
        currentLine = InputState.currentLine input
        line = input.position.line
        coloumn = input.position.coloumn
    }

    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition

    type Parser<'a> = {
        parseFn : InputState -> ParseResult<'a * InputState>
        label   : ParserLabel
    }

    let printResult =
        function
        | Success v -> printfn "%A" v
        | Failure (label, error, parserPos) ->
            let errorLine = parserPos.currentLine
            let linePos = parserPos.line
            let colPos = parserPos.coloumn
            let failureCaret = sprintf "%*s^%s" colPos "" error
            printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

    let setLabel parser newLabel =
        let newInnerFn input =
            let result = parser.parseFn input
            match result with
            | Success s -> Success s
            | Failure (_, err, pos) -> Failure (newLabel, err, pos)

        { parseFn = newInnerFn; label = newLabel }

    let getLabel parser = parser.label

    let ( <?> ) p l = setLabel p l

    let runOnInput parser input = parser.parseFn input

    let run parser inputStr = InputState.fromString inputStr |> runOnInput parser 

    let bindP f p1 =
        let label = sprintf "bind operation from %s" (getLabel p1)
        
        let innerFn input =
            let result = runOnInput p1 input

            match result with
            | Failure (label, err, pos) -> Failure (label, err, pos)
            | Success (value, rem) ->
                let p2 = f value

                runOnInput p2 rem

        { parseFn = innerFn; label = label }

    let ( >>= ) p f = bindP f p

    let returnP x =
        let label = "return operation"

        let innerFn input =
            Success (x, input)

        { parseFn = innerFn; label = label }

    let satisfy p label =
        let innerFn input =
            let remainingInput, charOpt = InputState.nextChar input
            match charOpt with
            | None -> 
                let err = "No more input"
                let pos = parserPositionFromInputState input
                Failure (label, err, pos)
            | Some c ->
                if p c then
                    Success (c, remainingInput)
                else
                    let err = sprintf "Unexpected '%c'" c
                    let pos = parserPositionFromInputState input
                    Failure (label, err, pos)

        { parseFn = innerFn; label = label }
        
    let pchar charToMatch =
        let p c = (c = charToMatch)
        let label = sprintf "'%c'" charToMatch

        satisfy p label

    let andThen parser1 parser2 =
        let label = sprintf "%s and then %s" (getLabel parser1) (getLabel parser2)

        parser1 >>= (fun r1 ->
        parser2 >>= (fun r2 ->
        returnP ((r1, r2))))
        <?> label

    let ( .>>. ) p1 p2 = andThen p1 p2

    let orElse parser1 parser2 =
        let label = sprintf "%s or else %s" (getLabel parser1) (getLabel parser2)

        let innerFn input =
            let result1 = runOnInput parser1 input

            match result1 with
            | Success _ -> result1
            | Failure _ ->
                let result2 = runOnInput parser2 input

                match result2 with
                | Success _ -> result2
                | Failure (_, err, pos) -> Failure (label, err, pos)

        { parseFn = innerFn; label = label }

    let ( <|> ) p1 p2 = orElse p1 p2

    let choice ps = List.reduce (<|>) ps

    let anyOf cs = 
        let label = sprintf "any of %A" cs

        cs
        |> List.map pchar
        |> choice
        <?> label

    let mapP f parser = parser >>= (f >> returnP)

    let ( <!> ) f p = mapP f p

    let ( |>> ) p f = mapP f p
    
    let ( .>> ) p1 p2 = p1 .>>. p2 |>> fst

    let ( >>. ) p1 p2 = p1 .>>. p2 |>> snd

    let applyP fP xP =
        let label = sprintf "applying from %s to %s" (getLabel fP) (getLabel xP)

        fP >>= (fun f ->
        xP >>= (f >> returnP))
        <?> label

    let ( <*> ) fP xP = applyP fP xP

    let lift2 f xP yP =
        let label = sprintf "lifting %s to %s" (getLabel xP) (getLabel yP)

        returnP f
        <*> xP
        <*> yP
        <?> label

    let rec sequence parserList =
        let cons head tail = head::tail

        let consP = lift2 cons

        match parserList with
        | []    -> returnP []
        | p::ps -> consP p (sequence ps)

    let charListToString charList = charList |> List.toArray |> System.String

    let pstring (str : string) =
        let label = str

        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP charListToString
        <?> label

    let rec parseZeroOrMore parser input =
        let firstResult = runOnInput parser input

        match firstResult with
        | Failure _ -> ([], input)
        | Success (firstValue, inputAfterFirstValue) ->
            let (subsequentValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstValue
            let values = firstValue :: subsequentValues
            (values, remainingInput)

    let many parser =
        let label = sprintf "many %s" (getLabel parser)
        let innerFn input = parseZeroOrMore parser input |> Success

        { parseFn = innerFn; label = label }

    let many1 parser =
        let label = sprintf "at least one %s" (getLabel parser)

        parser      >>= (fun head ->
        many parser >>= (fun tail ->
        head :: tail |> returnP))
        <?> label

    let manyChars1 c = many1 c |>> charListToString

    let whitespaceChar = 
        let p = System.Char.IsWhiteSpace
        let label = "whitespace"

        satisfy p label

    let spaces = many whitespaceChar

    let spaces1 = many1 whitespaceChar

    let opt p =
        let some = p |>> Some
        let none = returnP None

        some <|> none

    let digit =
        let p = System.Char.IsDigit
        let label = "digit"
        satisfy p label

    let pint =
        let label = "int"

        let resultsToInt (sign, str) = 
            let i = str |> int
            match sign with
            | Some _ -> -i
            | None   -> i

        let digits = manyChars1 digit

        pchar '-'
        |> opt
        .>>. digits
        |> mapP resultsToInt
        <?> label

    let pfloat =
        let label = "float"

        let resultsToFloat (sign, str) =
            let fl = str |> float
            match sign with
            | Some _ -> -fl
            | None   -> fl

        let digits = manyChars1 digit

        let decimals = 
            digits
            .>>. pchar '.'
            .>>. digits
            |>> (fun ((d1, c), d2) -> sprintf "%s%c%s" d1 c d2)
        
        pchar '-'
        |> opt
        .>>. decimals
        |> mapP resultsToFloat
        <?> label

