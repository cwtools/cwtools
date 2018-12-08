open System
[<Struct>]
type DU = | A of test:int | B of test2:bool | C | D | E | F | G
let stopwatch = new System.Diagnostics.Stopwatch()
let stopwatchInner = new System.Diagnostics.Stopwatch()

let inline inner v b c d e f  =
    stopwatch.Start()
    let res =
        match (v) with
        |A a -> stopwatchInner.Start(); a = a
        |B b -> b = b //'c' < 'a'
        |C -> 'd' > 'a'
        |D -> 'b' > 'r'
        |E -> true
        |F -> (fun () -> 'a' > 'a')()
        |G -> false
    stopwatch.Stop()
    stopwatchInner.Stop()
    res

let seq1 = seq { for i in 1 .. 10000000 -> (i) }
for (a) in seq1 do
    inner (A 2) [] [] [] [] []

printfn "%A %A" stopwatch.ElapsedMilliseconds stopwatchInner.ElapsedMilliseconds