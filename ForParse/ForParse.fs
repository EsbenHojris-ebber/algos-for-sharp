(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

    let run parser input =
        let (Parser innerFn) = parser
        innerFn input

    let pchar charToMatch =
        let innerFn str =
            if System.String.IsNullOrEmpty(str) then
                Failure "No more input"
            else
                let first = str.[0]
                if first = charToMatch then
                    let rem = str.[1..]
                    Success (charToMatch, rem)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Failure msg
        
        Parser innerFn

    let andThen parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Failure err -> Failure err
            | Success (value1, remaining1) ->
                let result2 = run parser2 remaining1

                match result2 with
                | Failure err -> Failure err
                | Success (value2, remaining2) ->
                    let newValue = (value1, value2)
                    Success (newValue, remaining2)

        Parser innerFn

    let ( .>>. ) p1 p2 = andThen p1 p2

    let orElse parser1 parser2 =
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Success _ -> result1
            | Failure _ ->
                let result2 = run parser2 input

                result2

        Parser innerFn

    let ( <|> ) p1 p2 = orElse p1 p2

    let choice ps = List.reduce (<|>) ps

    let anyOf cs = 
        cs
        |> List.map pchar
        |> choice

    let mapP f parser =
        let innerFn input =
            let result = run parser input

            match result with
            | Success (value, rem) ->
                let newValue = f value
                Success (newValue, rem)
            | Failure err -> Failure err

        Parser innerFn

    let ( <!> ) f p = mapP f p

    let ( |>> ) p f = mapP f p

    let returnP x =
        let innerFn input =
            Success (x, input)

        Parser innerFn

    let applyP fP xP =
        (fP .>>. xP)
        |> mapP (fun (f, x) -> f x)

    let ( <*> ) fP xP = applyP fP xP

    let lift2 f xP yP =
        returnP f
        <*> xP
        <*> yP

    let rec sequence parserList =
        let cons head tail = head::tail

        let consP = lift2 cons

        match parserList with
        | []    -> returnP []
        | p::ps -> consP p (sequence ps)

    let charListToStr charList = charList |> List.toArray |> System.String

    let pstring (str : string) =
        str
        |> List.ofSeq
        |> List.map pchar
        |> sequence
        |> mapP charListToStr

    let rec parseZeroOrMore parser input =
        let firstResult = run parser input
        match firstResult with
        | Failure _ -> ([], input)
        | Success (firstValue, inputAfterFirstValue) ->
            let (subsequentValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstValue
            let values = firstValue :: subsequentValues
            (values, remainingInput)

    let many parser =
        let innerFn input =
            Success (parseZeroOrMore parser input)

        Parser innerFn

    let whitespaceChar = anyOf [' '; '\t'; '\n']
    let whitespace = many whitespaceChar

    let many1 parser =
        let innerFn input =
            let firstResult = run parser input
            match firstResult with
            | Failure err -> Failure err
            | Success (firstValue, remAftFirst) ->
                let (subsequentValues, remainingInput) =
                    parseZeroOrMore parser remAftFirst
                let values = firstValue :: subsequentValues
                Success (values, remainingInput)
            
        Parser innerFn

    let opt p =
        let some = p |>> Some
        let none = returnP None
        some <|> none

    let pint =
        let resultsToInt (sign, charList) = 
            let i = charList |> List.toArray |> System.String |> int
            match sign with
            | Some _ -> -i
            | None   -> i
        let digit = anyOf ['0' .. '9']
        let digits = many1 digit

        pchar '-' |> opt
        .>>. digits
        |> mapP resultsToInt

