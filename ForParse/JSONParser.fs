module JSONParser

open ForParse.Parser

type JValue =
    | JString of string
    | JNumber of float
    | JBool of bool
    | JNull
    | JObject of Map<string, JValue>
    | JArray of JValue list

let ( >>% ) p x = p |>> (fun _ -> x)

let jNull =
    pstring "null"
    >>% JNull
    <?> "null"

let jBool =
    let ptrue =
        pstring "true"
        >>% JBool true
    
    let pfalse =
        pstring "false"
        >>% JBool false

    ptrue <|> pfalse
    <?> "bool"

let jString =
    let jUnescapedChar =
        let label = "char"
        satisfy (fun c -> c <> '\\' && c <> '\"') label
    
    let jEscapedChar =
        [
            ("\\\"", '\"')
            ("\\\\", '\\')
            ("\\/", '/')
            ("\\b", '\b')
            ("\\f", '\f')
            ("\\n", '\n')
            ("\\r", '\r')
            ("\\t", '\t')
        ]
        |> List.map (fun (toMatch, result) ->
            pstring toMatch >>% result)
        |> choice
        <?> "escaped char"

    let jUnicodeChar =
        let backslash = pchar '\\'
        let uChar = pchar 'u'
        let hexdigit = anyOf (['0' .. '9'] @ ['a' .. 'f'] @ ['A' .. 'F'])
        let fourHexdigits = hexdigit .>>. hexdigit .>>. hexdigit .>>. hexdigit 

        let convertToChar (((h1, h2), h3), h4)=
            let str = sprintf "%c%c%c%c" h1 h2 h3 h4
            System.Int32.Parse(str, System.Globalization.NumberStyles.HexNumber) |> char

        backslash >>. uChar >>. fourHexdigits
        |>> convertToChar
    
    let quotedString = 
        let quote = pchar '\"' <?> "quote"
        let jChar = jUnescapedChar <|> jEscapedChar <|> jUnicodeChar

        quote >>. manyChars jChar .>> quote

    quotedString
    |>> JString
    <?> "string"

let JNumber =
    let optSign = pchar '-' |> opt

    let zero = pstring "0"

    let digitOneNine = satisfy (fun c -> System.Char.IsDigit c && c <> '0') "digit"

    let point = pchar '.'

    let e = pchar 'e' <|> pchar 'e'

    let optPlusMinus = pchar '+' <|> pchar '-'

    let nonZeroInt = 
        digitOneNine
        .>>. manyChars digit
        |>> fun (first, rest) -> string first + rest

    let intPart = zero <|> nonZeroInt

    let fractionPart = point >>. manyChars1 digit

    let exponentPart = e >>. optSign .>>. manyChars1 digit

    let ( |>? ) opt f =
        match opt with
        | None -> ""
        | Some x -> f x

    let convertToJNumber (((optSign, intPart), fractionPart), exponentPart) =
        let signStr = optSign |>? string

        let fractionPartStr =
            fractionPart 
            |>? fun digits -> "." + digits

        let exponentPartStr =
            exponentPart
            |>? (fun (optSign, digits) -> 
                let sign = optSign |>? string
                "e" + sign + digits)

        (signStr + intPart + fractionPartStr + exponentPartStr)
        |> float
        |> JNumber

    optSign .>>. intPart .>>. opt fractionPart .>>. opt exponentPart
    |>> convertToJNumber
    .>> spaces1
    <?> "number"
