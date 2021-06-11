(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string

    type Parser<'T> = Parser of (string -> ParseResult<'T * string>)

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

    let run parser input =
        let (Parser innerFn) = parser
        innerFn input
