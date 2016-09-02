open Configuration

[<EntryPoint>]
let main argv = 
    Config.Run argv
    //stdin.ReadLine() |> ignore
    0 // return an integer exit code
