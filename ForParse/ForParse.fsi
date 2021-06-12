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
    val anyOf   : char list -> Parser<char>

    val andThen : Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    val orElse  : Parser<'a> -> Parser<'a> -> Parser<'a>
    val choice  : Parser<'a> list -> Parser<'a>
    val mapP    : ('a -> 'b) -> Parser<'a> -> Parser<'b>
    val returnP : 'a -> Parser<'a>
    val applyP  : Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>
    val lift2   : ('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c>
    val sequence: Parser<'a> list -> Parser<'a list>

    val ( .>>. ): Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    val ( <|> ) : Parser<'a> -> Parser<'a> -> Parser<'a>
    val ( <!> ) : ('a -> 'b) -> Parser<'a> -> Parser<'b>
    val ( |>> ) : Parser<'a> -> ('a -> 'b) -> Parser<'b>
    val ( <*> ) : Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>

    val run     : Parser<'a> -> string -> ParseResult<'a * string>