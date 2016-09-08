module Generators 
let sequentialGenerator first = 
    let mutable n = first
    fun () -> 
        n <- n + 1
        n |> string

let textualField s = sprintf "'%s'" s

let decimalGenerator() = 
    let r = System.Random()
    let getMax n = Seq.init n (fun _ -> 10m) |> Seq.fold (*) 1m
    fun precision scale -> (r.NextDouble() |> decimal) * ((getMax precision) / (getMax scale)) |> string

let floatGenerator() = 
    let r = System.Random()
    let getMax n = Seq.init n (fun _ -> 10.) |> Seq.fold (*) 1.
    fun precision -> r.NextDouble() * (getMax precision) |> string

let integerGenerator() = 
    let r = System.Random()
    let getMax n = Seq.init n (fun _ -> 10I) |> Seq.fold (*) 1I
    fun precision -> (r.Next() |> bigint) % (getMax precision) |> string

let dateGenerator() = 
    let r = System.Random()
    fun () -> System.DateTime.Today.AddHours((r.NextDouble() - 0.5) * 58000.).ToString("yyyy-MM-dd") |> textualField

let timeGenerator() = 
    let r = System.Random()
    fun () -> System.DateTime.Today.AddHours((r.NextDouble() - 0.5) * 58000.).ToString("hh:mm:ss") |> textualField

let dateTimeGenerator() = 
    let dGen = dateGenerator()
    let tGen = timeGenerator()
    fun () -> (dGen() + " " + tGen()) |> textualField

let characterGenerator() = 
    let r = System.Random()
    let baseLower = int 'a'
    let baseUpper = int 'A'
    fun () -> 
        match r.Next(52) with
        | n when n > 25 -> 
            baseLower + (n % 26)
            |> char
            |> string
        | n -> 
            baseUpper + n
            |> char
            |> string

let stringGenerator names = 
    let r = System.Random()
    
    let list = 
        match names |> Seq.isEmpty with
        | true -> 
            let cGen = characterGenerator()
            Array.init 100 (fun _ -> Seq.init (r.Next(5) + 5) (fun _ -> cGen()) |> String.concat "")
        | false -> names |> Seq.toArray
    
    let len = list.Length
    fun maxlen -> 
        list.[r.Next(len)]
        |> Seq.truncate maxlen
        |> Seq.toArray
        |> System.String
        |> string |> textualField
