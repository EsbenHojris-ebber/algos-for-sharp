(*
    Module build as per the F# for fun and profit parser series
*)
namespace ForParse

module Parser =
    let parseA str =
        if System.String.IsNullOrEmpty(str) then
            (false, "")
        else if str.[0] = 'A' then
            let remaining = str.[1..]
            (true, remaining)
        else
            (false, str)
