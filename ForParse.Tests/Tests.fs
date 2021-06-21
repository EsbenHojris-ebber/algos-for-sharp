module Tests

open System
open Xunit

open ForParse.Parser

[<Theory>]
[<InlineData('A', "ABC")>]
[<InlineData('B', "BC")>]
[<InlineData('A', "AAAAC")>]
let ``Parse char - success`` (c, inp) =
    let act = run (pchar c) inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = 1
        }
    }
    Assert.Equal (Success (c, inpSt), act)

[<Theory>]
[<InlineData('A', "BC", "Unexpected 'B'")>]
[<InlineData('A', "", "No more input")>]
[<InlineData('B', "AAAAC", "Unexpected 'A'")>]
let ``Parse char - failure`` (c, inp, errmsg) =
    let act = run (pchar c) inp
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = 0
    }
    Assert.Equal (Failure (sprintf "'%c'" c, errmsg, parPos), act)

[<Theory>]
[<InlineData('A', 'B', "ABC")>]
[<InlineData('B', 'C', "BC")>]
[<InlineData('A', 'A', "AAAAC")>]
let ``Parse 'and' - success`` (c1, c2, inp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 .>>. parse2

    let act = run parse1and2 inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = 2
        }
    }
    Assert.Equal (Success ((c1, c2), inpSt), act)

[<Theory>]
[<InlineData('A', 'A', "ABC", "Unexpected 'B'", 1)>]
[<InlineData('B', 'D', "BC", "Unexpected 'C'", 1)>]
[<InlineData('A', 'A', "", "No more input", 0)>]
[<InlineData('A', 'A', "A", "Unexpected '\n'", 1)>]
let ``Parse 'and' - failure`` (c1, c2, inp, errmsg, errcol) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 .>>. parse2

    let act = run parse1and2 inp
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = errcol
    }
    Assert.Equal (Failure (sprintf "'%c' and then '%c'" c1 c2, errmsg, parPos), act)

[<Theory>]
[<InlineData('A', 'B', "ABC")>]
[<InlineData('B', 'C', "BC")>]
[<InlineData('A', 'D', "AAAAC")>]
let ``Parse 'or' first - success`` (c1, c2, inp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1or2 = parse1 <|> parse2

    let act = run parse1or2 inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = 1
        }
    }
    Assert.Equal (Success (c1, inpSt), act)

[<Theory>]
[<InlineData('A', 'B', "BBC")>]
[<InlineData('C', 'B', "BC")>]
[<InlineData('B', 'A', "AAAAC")>]
let ``Parse 'or' second - success`` (c1, c2, inp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1or2 = parse1 <|> parse2

    let act = run parse1or2 inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = 1
        }
    }
    Assert.Equal (Success (c2, inpSt), act)

[<Theory>]
[<InlineData('B', 'C', "ABC", "Unexpected 'A'")>]
[<InlineData('A', 'D', "BC", "Unexpected 'B'")>]
[<InlineData('B', 'C', "", "No more input")>]
let ``Parse two or - failure`` (c1, c2, inp, msg) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 <|> parse2

    let act = run parse1and2 inp
    let label = sprintf "%s or else %s" (getLabel parse1) (getLabel parse2)
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = 0
    }
    Assert.Equal (Failure (label, msg, parPos), act)

let parseThreeDigits = 
    let digit = anyOf ['0'..'9']

    digit .>>. digit .>>. digit
    |>> fun ((c1, c2), c3) -> System.String [|c1; c2; c3|]
    <?> "three digits"

[<Theory>]
[<InlineData("425DV", "425")>]
[<InlineData("5784XV", "578")>]
[<InlineData("426", "426")>]
let ``Parse three digits - success`` (inp, value) =
    let act = run parseThreeDigits inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = 3
        }
    }
    Assert.Equal (Success (value, inpSt), act)

[<Theory>]
[<InlineData("42DV", "Unexpected 'D'", 2)>]
[<InlineData("57", "Unexpected '\n'", 2)>]
let ``Parse three digits - failure`` (inp, errmsg, errpos) =
    let act = run parseThreeDigits inp
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = errpos
    }
    Assert.Equal (Failure ("three digits", errmsg, parPos), act)

[<Theory>]
[<InlineData("ABCDE", "ABC", 3)>]
[<InlineData("ABC", "ABC", 3)>]
[<InlineData("wauw DE", "wauw", 4)>]
let ``Parse string - success`` (inp, value, endPos) =
    let parser = pstring value
    let act = run parser inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = endPos
        }
    }
    Assert.Equal (Success(value, inpSt), act)

[<Theory>]
[<InlineData("ABDE", "ABC", "Unexpected 'D'", 2)>]
[<InlineData("AB", "ABC", "Unexpected '\n'", 2)>]
[<InlineData("wau DE", "wauw", "Unexpected ' '", 3)>]
let ``Parse string - failure`` (inp, pat, errmsg, errpos) =
    let parser = pstring pat
    let act = run parser inp
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = errpos
    }
    Assert.Equal (Failure (pat, errmsg, parPos), act)

[<Theory>]
[<InlineData("ABDE", "A", 1)>]
[<InlineData("AAAB", "AAA", 3)>]
[<InlineData("CCDB", "", 0)>]
[<InlineData("AAA", "AAA", 3)>]
let ``Parse many chars`` (inp, value, endPos) =
    let parser = pchar 'A' |> many |>> (List.toArray >> String)
    let act = run parser inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = endPos
        }
    }
    Assert.Equal (Success (value, inpSt), act)

[<Theory>]
[<InlineData("AB", "ABABDE", "ABAB", 4)>]
[<InlineData("AA", "AAAB", "AA", 2)>]
[<InlineData("CCD", "CCDCCDB", "CCDCCD", 6)>]
[<InlineData("fjkldsnm", "AAA", "", 0)>]
let ``Parse many strings`` (pat, inp, value, endPos) =
    let parser = pstring pat |> many |>> String.concat ""
    let act = run parser inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = endPos
        }
    }
    Assert.Equal (Success (value, inpSt), act)

[<Theory>]
[<InlineData("1ABDE", 1, 1)>]
[<InlineData("32", 32, 2)>]
[<InlineData("4276S321", 4276, 4)>]
[<InlineData("0AAA", 0, 1)>]
[<InlineData("-67AAA", -67, 3)>]
let ``Parse int (many1) - success`` (inp, value, endPos) =
    let parser = pint
    let act = run parser inp
    let inpSt = {
        lines = [| inp |]
        position = {
            line = 0
            coloumn = endPos
        }
    }
    Assert.Equal (Success (value, inpSt), act)

[<Theory>]
[<InlineData("ABDE", "Unexpected 'A'", 0)>]
[<InlineData("", "No more input", 0)>]
[<InlineData("S321", "Unexpected 'S'", 0)>]
[<InlineData("-S321", "Unexpected 'S'", 1)>]
let ``Parse int (many1) - failure`` (inp, errmsg, errpos) =
    let parser = pint
    let act = run parser inp
    let parPos = {
        currentLine = match inp with | "" -> "end of file" | _ -> inp
        line = 0
        coloumn = errpos
    }
    Assert.Equal (Failure ("int", errmsg, parPos), act)

let parseDigitWithLabel = anyOf ['0' .. '9'] <?> "digit"

[<Fact>]
let ``Parse with label`` =
    let act = run parseDigitWithLabel "|ABC"
    let parPos = {
        currentLine = "|ABC"
        line = 0
        coloumn = 0
    }
    Assert.Equal (Failure("digit", "Unexpected '|'", parPos), act)