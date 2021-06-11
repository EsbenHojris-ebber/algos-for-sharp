module Tests

open System
open Xunit

open ForParse

[<Fact>]
let ``Parse A`` () =
    let exp = (true, "BC")
    let act = Parser.parseA "ABC"
    Assert.Equal (exp, act)
