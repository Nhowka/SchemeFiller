



[<EntryPoint>]
let main argv = 
    let insertCreator = InsertCreator.insertCreator []
    (SchemeLoader.load "127.0.0.1" "root" "" "dbrecebimentonovo") |> Seq.map insertCreator |> Seq.iter (printfn "%s")
    //stdin.ReadLine() |> ignore
    0 // return an integer exit code
