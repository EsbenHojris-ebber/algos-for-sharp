module Tests

open System
open Xunit

open ForParse.Parser

[<Theory>]
[<InlineData('A', "ABC", "BC")>]
[<InlineData('B', "BC", "C")>]
[<InlineData('A', "AAAAC", "AAAC")>]
let ``Parse any - success`` (c, inp, exp) =
    let act = run (pchar c) inp
    Assert.Equal (Success (c, exp), act)

[<Theory>]
[<InlineData('A', "BC", "Expecting 'A'. Got 'B'")>]
[<InlineData('A', "", "No more input")>]
[<InlineData('B', "AAAAC", "Expecting 'B'. Got 'A'")>]
let ``Parse any - failure`` (c, inp, msg) =
    let act = run (pchar c) inp
    Assert.Equal (Failure msg, act)

[<Theory>]
[<InlineData('A', 'B', "ABC", "C")>]
[<InlineData('B', 'C', "BC", "")>]
[<InlineData('A', 'A', "AAAAC", "AAC")>]
let ``Parse two and - success`` (c1, c2, inp, exp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 .>>. parse2

    let act = run parse1and2 inp
    Assert.Equal (Success ((c1, c2), exp), act)

[<Theory>]
[<InlineData('A', 'A', "ABC", "Expecting 'A'. Got 'B'")>]
[<InlineData('B', 'D', "BC", "Expecting 'D'. Got 'C'")>]
[<InlineData('A', 'A', "", "No more input")>]
[<InlineData('A', 'A', "A", "No more input")>]
let ``Parse two and - failure`` (c1, c2, inp, msg) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 .>>. parse2

    let act = run parse1and2 inp
    Assert.Equal (Failure msg, act)

[<Theory>]
[<InlineData('A', 'B', "ABC", "BC")>]
[<InlineData('B', 'C', "BC", "C")>]
[<InlineData('A', 'D', "AAAAC", "AAAC")>]
let ``Parse two or first - success`` (c1, c2, inp, exp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 <|> parse2

    let act = run parse1and2 inp
    Assert.Equal (Success (c1, exp), act)

[<Theory>]
[<InlineData('A', 'B', "BBC", "BC")>]
[<InlineData('C', 'B', "BC", "C")>]
[<InlineData('B', 'A', "AAAAC", "AAAC")>]
let ``Parse two or second - success`` (c1, c2, inp, exp) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 <|> parse2

    let act = run parse1and2 inp
    Assert.Equal (Success (c2, exp), act)

[<Theory>]
[<InlineData('B', 'C', "ABC", "Expecting 'C'. Got 'A'")>]
[<InlineData('A', 'D', "BC", "Expecting 'D'. Got 'B'")>]
[<InlineData('B', 'C', "", "No more input")>]
let ``Parse two or - failure`` (c1, c2, inp, msg) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 <|> parse2

    let act = run parse1and2 inp
    Assert.Equal (Failure msg, act)

let parseThreeDigits = 
    let digit = anyOf ['0'..'9']

    digit .>>. digit .>>. digit
    |>> fun ((c1, c2), c3) -> System.String [|c1; c2; c3|]

[<Theory>]
[<InlineData("425DV", "425", "DV")>]
[<InlineData("5784XV", "578", "4XV")>]
[<InlineData("426", "426", "")>]
let ``Parse three digits - success`` (inp, value, rem) =
    let act = run parseThreeDigits inp
    Assert.Equal (Success (value, rem), act)

[<Theory>]
[<InlineData("42DV", "Expecting '9'. Got 'D'")>]
[<InlineData("57", "No more input")>]
let ``Parse three digits - failure`` (inp, msg) =
    let act = run parseThreeDigits inp
    Assert.Equal (Failure msg, act)

[<Theory>]
[<InlineData("ABCDE", "ABC", "DE")>]
[<InlineData("ABC", "ABC", "")>]
[<InlineData("wauw DE", "wauw", " DE")>]
let ``Parse string - success`` (inp, value, rem) =
    let parser = pstring value
    let act = run parser inp
    Assert.Equal (Success(value, rem), act)

[<Theory>]
[<InlineData("ABDE", "ABC", "Expecting 'C'. Got 'D'")>]
[<InlineData("AB", "ABC", "No more input")>]
[<InlineData("wau DE", "wauw", "Expecting 'w'. Got ' '")>]
let ``Parse string - failure`` (inp, pat, msg) =
    let parser = pstring pat
    let act = run parser inp
    Assert.Equal (Failure msg, act)

[<Theory>]
[<InlineData("ABDE", "A", "BDE")>]
[<InlineData("AAAB", "AAA", "B")>]
[<InlineData("CCDB", "", "CCDB")>]
[<InlineData("AAA", "AAA", "")>]
let ``Parse many chars`` (inp, value, rem) =
    let parser = pchar 'A' |> many |>> (List.toArray >> String)
    let act = run parser inp
    Assert.Equal (Success (value, rem), act)

[<Theory>]
[<InlineData("AB", "ABABDE", "ABAB", "DE")>]
[<InlineData("AA", "AAAB", "AA", "AB")>]
[<InlineData("CCD", "CCDCCDB", "CCDCCD", "B")>]
[<InlineData("fjkldsnm", "AAA", "", "AAA")>]
let ``Parse many strings`` (pat, inp, value, rem) =
    let parser = pstring pat |> many |>> String.concat ""
    let act = run parser inp
    Assert.Equal (Success (value, rem), act)

[<Theory>]
[<InlineData("1ABDE", 1, "ABDE")>]
[<InlineData("32", 32, "")>]
[<InlineData("4276S321", 4276, "S321")>]
[<InlineData("0AAA", 0, "AAA")>]
let ``Parse many1 digits - success`` (inp, value, rem) =
    let parser = pint
    let act = run parser inp
    Assert.Equal (Success (value, rem), act)

[<Theory>]
[<InlineData("ABDE", "Expecting '9'. Got 'A'")>]
[<InlineData("", "No more input")>]
[<InlineData("S321", "Expecting '9'. Got 'S'")>]
let ``Parse many1 digits - failure`` (inp, msg) =
    let parser = pint
    let act = run parser inp
    Assert.Equal (Failure msg, act)