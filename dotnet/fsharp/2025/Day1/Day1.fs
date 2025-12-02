module fsharp._2025.Day1.Day1

open System.IO
open Xunit
open Faqt

let parseInput filePath =
    let lines = File.ReadLines filePath
    lines |> Seq.map (fun line -> (line[0], int line[1..])) |> Seq.toArray

[<Fact>]
let ``part1`` () =
    let input = parseInput "2025/Day1/Data.txt"

    let _, counter =
        input
        |> Array.fold
            (fun (position, count) (turn, distance) ->
                let offset = if turn = 'R' then distance else -distance
                let nextPosition = ((position + offset) % 100 + 100) % 100
                let nextCount = if nextPosition = 0 then count + 1 else count
                (nextPosition, nextCount)
            )
            (50, 0)

    counter.Should().Be 1007

[<Fact>]
let ``part2`` () =
    let input = parseInput "2025/Day1/Data.txt"

    let _, counter =
        input
        |> Array.fold
            (fun (position, count) (turn, distance) ->
                // Number of full circles
                let fullCircles = distance / 100

                let remainder =
                    if turn = 'R' then
                        // Check if the remaining rotation crosses 0
                        let remainderCrosses = if position + (distance % 100) >= 100 then 1 else 0
                        remainderCrosses
                    else // turn = 'L'
                        // Check if the remaining rotation crosses 0.
                        // It crosses if the distance is enough to go past 0, but not if it starts at 0.
                        let remainderCrosses =
                            if position > 0 && (distance % 100) >= position then 1 else 0

                        remainderCrosses

                let nextPosition =
                    if turn = 'R' then
                        (position + distance) % 100
                    else
                        // ?????? why does modulo not work properly for negative numbers
                        ((position - distance) % 100 + 100) % 100

                (nextPosition, count + fullCircles + remainder)
            )
            (50, 0)

    counter.Should().Be (5820)
