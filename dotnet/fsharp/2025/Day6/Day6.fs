module fsharp._2025.Day6.Day6

open System
open System.IO
open System.Text.RegularExpressions
open Xunit
open Expecto

let parseInput1 filePath =
    let lines = File.ReadAllLines filePath

    lines
    |> Array.map (fun line ->
        line.Trim().Split ' ' |> Array.filter (fun x -> x |> String.IsNullOrWhiteSpace |> not)
    )
    |> Array.transpose
    |> Array.map (fun x ->
        let tillLast = x[.. x.Length - 2] |> Array.map bigint.Parse
        let operator = x |> Array.last
        tillLast, operator |> char
    )

[<Fact>]
let ``part1`` () =
    let input = parseInput1 "2025/Day6/Data.txt"

    input
    |> Array.map (fun (numbers, operator) ->
        match operator with
        | '+' -> numbers |> Array.sum
        | '*' -> numbers |> Array.reduce (*)
        | _ -> failwith "Invalid operator"
    )
    |> Array.sum
    |> Flip.Expect.equal "equal" 5346286649122I

let parseInputPart2 filePath =
    let lines = File.ReadAllLines filePath

    let maxLen = lines |> Array.map String.length |> Array.max
    let paddedLines = lines |> Array.map _.PadRight(maxLen)

    let columns =
        [| 0 .. maxLen - 1 |] //
        |> Array.map (fun i -> paddedLines |> Array.map (fun line -> line[i]))

    // Split into problems at space-only columns
    let problems =
        columns
        |> Array.fold
            (fun (acc, current) col ->
                if col |> Array.forall ((=) ' ') then
                    if current |> List.isEmpty then (acc, [])
                    else (acc @ [current |> List.rev |> List.toArray], [])
                else
                    (acc, col :: current)
            )
            ([], [])
        |> fun (acc, current) ->
            if current |> List.isEmpty then acc
            else acc @ [current |> List.rev |> List.toArray]
        |> List.toArray

    problems
    |> Array.map (fun cols ->
        let operator =
            cols
            |> Array.map Array.last
            |> Array.find ((<>) ' ')

        let numbers =
            cols
            |> Array.map (fun col ->
                let digits = col[.. col.Length - 2] |> Array.filter Char.IsDigit
                String(digits) |> bigint.Parse
            )

        numbers, operator
    )


[<Fact>]
let ``part2`` () =
    let input = parseInputPart2 "2025/Day6/Data.txt"

    input
    |> Array.map (fun (numbers, operator) ->
        match operator with
        | '+' -> numbers |> Array.sum
        | '*' -> numbers |> Array.reduce (*)
        | _ -> failwith "Invalid operator"
    )
    |> Array.sum
    |> Flip.Expect.equal "equal" 10389131401929I
