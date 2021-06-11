module Tests

open System
open Xunit

open ForParse

[<Theory>]
[<InlineData("ABC", true, "BC")>]
[<InlineData("BC", false, "BC")>]
[<InlineData("A", true, "")>]
[<InlineData("", false, "")>]
[<InlineData("AAAAC", true, "AAAC")>]
let ``Parse A`` (inp, exp1, exp2) =
    let act = Parser.parseA inp
    Assert.Equal ((exp1, exp2), act)
