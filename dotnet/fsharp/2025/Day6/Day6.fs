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

let parseInput2 filePath =
    let lines = File.ReadAllLines filePath

    // Pad lines to same length and transpose to get columns
    let maxLen = lines |> Array.map String.length |> Array.max
    let paddedLines = lines |> Array.map _.PadRight(maxLen)

    let columns =
        [| 0 .. maxLen - 1 |] |> Array.map (fun i -> paddedLines |> Array.map (fun line -> line[i]))

    // Group columns by problems (space-only columns are separators)
    let problems =
        columns
        |> Array.fold
            (fun (acc, current) col ->
                if col |> Array.forall (fun c -> c = ' ') then
                    if current |> List.isEmpty then
                        (acc, [])
                    else
                        ((current |> List.rev) :: acc, [])
                else
                    (acc, col :: current)
            )
            ([], [])
        |> fun (acc, current) ->
            if current |> List.isEmpty then acc else (current |> List.rev) :: acc
        |> List.map List.toArray
        |> List.toArray

    // Parse each problem: columns become numbers, last char of each column is operator
    problems
    |> Array.map (fun cols ->
        let operator =
            cols //
            |> Array.map (fun y -> y |> Array.last)
            |> Array.find (fun c -> c <> ' ')
            |> char

        let numbers =
            cols
            |> Array.map (fun col ->
                // Read digits top-to-bottom, ignoring spaces and operator row
                let digits = col[.. col.Length - 2] |> Array.filter Char.IsDigit
                new String (digits) |> bigint.Parse
            )

        numbers, operator
    )


[<Fact>]
let ``part2`` () =
    let input = parseInput2 "2025/Day6/Data.txt"

    input
    |> Array.map (fun (numbers, operator) ->
        match operator with
        | '+' -> numbers |> Array.sum
        | '*' -> numbers |> Array.reduce (*)
        | _ -> failwith "Invalid operator"
    )
    |> Array.sum
    |> Flip.Expect.equal "equal" 10389131401929I
