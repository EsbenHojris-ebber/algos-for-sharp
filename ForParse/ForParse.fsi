(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type ParseResult<'a> =
        | Success of 'a
        | Failure of string
    
    type Parser<'T>

    val pchar   : char -> Parser<char>

    val andThen : Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    val ( .>>. ): Parser<'a> -> Parser<'b> -> Parser<'a * 'b>

    val run     : Parser<'a> -> string -> ParseResult<'a * string>