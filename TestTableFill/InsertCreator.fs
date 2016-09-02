module InsertCreator
open Definitions
open Generators

let insertCreator names = 
    let sGen = stringGenerator names
    let dGen = decimalGenerator()
    let fGen = floatGenerator()
    let cGen = characterGenerator()
    let iGen = integerGenerator()
    let dtGen = dateGenerator()
    let tGen = timeGenerator()
    let dttGen = dateTimeGenerator()
    fun table -> 
        let { Name = tab; Columns = c } = table
        c
        |> List.map (fun d -> 
               let name = d.Name
               
               let value = 
                   match d.Primary, d.Type with
                   | Yes g, _ -> g()
                   | No, Text s -> sGen s
                   | No, Date -> dtGen()
                   | No, DateTime -> dttGen()
                   | No, Decimal(m, s) -> dGen m s
                   | No, Float m -> fGen m
                   | No, Integer i -> iGen i
                   | No, Time -> tGen()
                   | No, TimeStamp -> dttGen()
                   | No, Unknown -> "null"
               (name |> sprintf "`%s`", value |> sprintf "'%s'"))
        |> List.unzip
        |> (fun f (v1, v2) -> (f v1, f v2)) (String.concat ", ")
        ||> sprintf "INSERT INTO %s (%s) VALUES (%s);" tab
