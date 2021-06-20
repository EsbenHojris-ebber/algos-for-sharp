(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type ParserLabel = string
    type ParserError = string

    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError

    type Parser<'a> = {
        parseFn : string -> ParseResult<'a * string>
        label   : ParserLabel
    }

    let printResult =
        function
        | Success v -> printfn "%A" v
        | Failure (label, error) -> printfn "Error parsing %s\n%s" label error

    let run parser input =
        parser.parseFn input

    let bindP f p1 =
        let label = "unknown"
        let innerFn input =
            let result = run p1 input

            match result with
            | Failure (label, err) -> Failure (label, err)
            | Success (value, rem) ->
                let p2 = f value

                run p2 rem

        { parseFn = innerFn; label = label }

    let ( >>= ) p f = bindP f p

    let returnP x =
        let label = "unknown"
        let innerFn input =
            Success (x, input)

        { parseFn = innerFn; label = label }
        
    let pchar charToMatch =
        let label = "unknown"
        let innerFn str =
            if System.String.IsNullOrEmpty(str) then
                Failure (str, "No more input")
            else
                let first = str.[0]
                if first = charToMatch then
                    let rem = str.[1..]
                    Success (charToMatch, rem)
                else
                    let msg = sprintf "Expecting '%c'. Got '%c'" charToMatch first
                    Failure (str, msg)
        
        { parseFn = innerFn; label = label }

    let andThen parser1 parser2 =
        parser1 >>= (fun r1 ->
        parser2 >>= (fun r2 ->
        returnP ((r1, r2))))

    let ( .>>. ) p1 p2 = andThen p1 p2

    let orElse parser1 parser2 =
        let label = "unknown"
        let innerFn input =
            let result1 = run parser1 input

            match result1 with
            | Success _ -> result1
            | Failure _ ->
                let result2 = run parser2 input

                result2

        { parseFn = innerFn; label = label }

    let ( <|> ) p1 p2 = orElse p1 p2

    let choice ps = List.reduce (<|>) ps

    let anyOf cs = 
        cs
        |> List.map pchar
        |> choice

    let mapP f parser = parser >>= (f >> returnP)

    let ( <!> ) f p = mapP f p

    let ( |>> ) p f = mapP f p
    
    let ( .>> ) p1 p2 = p1 .>>. p2 |>> fst

    let ( >>. ) p1 p2 = p1 .>>. p2 |>> snd

    let applyP fP xP =
        fP >>= (fun f ->
        xP >>= (f >> returnP))

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
        let label = "unknown"
        let innerFn input = parseZeroOrMore parser input |> Success

        { parseFn = innerFn; label = label }

    let whitespaceChar = anyOf [' '; '\t'; '\n']
    let whitespace = many whitespaceChar

    let many1 parser =
        parser      >>= (fun head ->
        many parser >>= (fun tail ->
        head :: tail |> returnP))

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

        pchar '-'
        |> opt
        .>>. digits
        |> mapP resultsToInt
