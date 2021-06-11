module Tests

open System
open Xunit

open ForParse

[<Theory>]
[<InlineData('A', "ABC", "BC")>]
[<InlineData('B', "BC", "C")>]
[<InlineData('A', "AAAAC", "AAAC")>]
let ``Parse any - success`` (c, inp, exp) =
    let act = Parser.run (Parser.pchar c) inp
    Assert.Equal (Parser.Success (c, exp), act)

[<Theory>]
[<InlineData('A', "BC", "Expecting 'A'. Got 'B'")>]
[<InlineData('A', "", "No more input")>]
[<InlineData('B', "AAAAC", "Expecting 'B'. Got 'A'")>]
let ``Parse any - failure`` (c, inp, msg) =
    let act = Parser.run (Parser.pchar c) inp
    Assert.Equal (Parser.Failure msg, act)