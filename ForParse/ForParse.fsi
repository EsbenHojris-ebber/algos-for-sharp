(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    type ParserLabel = string
    type ParserError = string
    type ParserPosition = {
        currentLine : string
        line    : int
        coloumn : int
    }

    type ParseResult<'a> =
        | Success of 'a
        | Failure of ParserLabel * ParserError * ParserPosition

    type Position = {
        line    : int
        coloumn : int
    }
    type InputState = {
        lines    : string[]
        position : Position
    }
    
    type Parser<'T>

    val whitespaceChar  : Parser<char>
    val spaces          : Parser<char list>
    val spaces1         : Parser<char list>
    val digit           : Parser<char>
    val pint            : Parser<int>
    val pfloat          : Parser<float>

    val satisfy : (char -> bool) -> ParserLabel -> Parser<char>
    val pchar   : char -> Parser<char>
    val pstring : string -> Parser<string>
    val anyOf   : char list -> Parser<char>
    val many    : Parser<'a> -> Parser<'a list>
    val many1   : Parser<'a> -> Parser<'a list>
    val manyChars1 : Parser<char> -> Parser<string>

    val bindP   : ('a -> Parser<'b>) -> Parser<'a> -> Parser<'b>
    val andThen : Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    val orElse  : Parser<'a> -> Parser<'a> -> Parser<'a>
    val choice  : Parser<'a> list -> Parser<'a>
    val mapP    : ('a -> 'b) -> Parser<'a> -> Parser<'b>
    val returnP : 'a -> Parser<'a>
    val applyP  : Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>
    val lift2   : ('a -> 'b -> 'c) -> Parser<'a> -> Parser<'b> -> Parser<'c>
    val sequence: Parser<'a> list -> Parser<'a list>
    val opt     : Parser<'a> -> Parser<'a option>

    val setLabel: Parser<'a> -> ParserLabel -> Parser<'a>
    val getLabel: Parser<'a> -> ParserLabel

    val ( >>= ) : Parser<'a> -> ('a -> Parser<'b>) -> Parser<'b>
    val ( .>>. ): Parser<'a> -> Parser<'b> -> Parser<'a * 'b>
    val ( .>> ) : Parser<'a> -> Parser<'b> -> Parser<'a>
    val ( >>. ) : Parser<'a> -> Parser<'b> -> Parser<'b>
    val ( <|> ) : Parser<'a> -> Parser<'a> -> Parser<'a>
    val ( <!> ) : ('a -> 'b) -> Parser<'a> -> Parser<'b>
    val ( |>> ) : Parser<'a> -> ('a -> 'b) -> Parser<'b>
    val ( <*> ) : Parser<'a -> 'b> -> Parser<'a> -> Parser<'b>

    val ( <?> ) : Parser<'a> -> ParserLabel -> Parser<'a>

    val run     : Parser<'a> -> string -> ParseResult<'a * InputState>
    val printResult : ParseResult<'a> -> unit

    val charListToString : char list -> string