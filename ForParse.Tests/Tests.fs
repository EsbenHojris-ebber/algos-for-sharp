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
let ``Parse two - success`` (c1, c2, inp, exp) =
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
let ``Parse two - failure`` (c1, c2, inp, msg) =
    let parse1 = pchar c1
    let parse2 = pchar c2
    let parse1and2 = parse1 .>>. parse2

    let act = run parse1and2 inp
    Assert.Equal (Failure msg, act)

