module InsertCreator
open Definitions
open Generators

let insertCreator names =
    let sGen = stringGenerator names
    let dGen = decimalGenerator()
    let fGen = floatGenerator()
    let iGen = integerGenerator()
    let dtGen = dateGenerator()
    let tGen = timeGenerator()
    let dttGen = dateTimeGenerator()
    fun table keyCache ->
        let { Name = tab; Columns = c } = table

        let rec permute total lists =
            match lists with
            | [] -> total
            | a :: b ->
                permute [ for i in a do
                              for j in total do
                                  yield i :: j ] b

        let constructFields foreign keyCache =
            let rec construct fields cols keyCache fkeys =
                match cols with
                | [] -> fields |> List.rev, keyCache
                | a :: b ->
                    let name = (a : Column).Name

                    let (value, newCache, newFks) =
                        match a.Primary, a.Type, a.Foreign with
                        | Yes _, _, Foreign.Yes _ ->
                            let v, nkeys =
                                match fkeys with
                                | [] -> "null", []
                                | a :: b -> a, b

                            let key = (a.Table, a.Name)

                            let values =
                                match keyCache |> Map.tryFind key with
                                | Some v -> v
                                | None -> []
                            v, keyCache |> Map.add key (v :: values), nkeys
                        | _, _, Foreign.Yes _ ->
                            match fkeys with
                            | [] -> "null", keyCache, []
                            | a :: b -> a, keyCache, b
                        | Yes g, _, _ ->
                            let v = g()
                            let key = (a.Table, a.Name)

                            let values =
                                match keyCache |> Map.tryFind key with
                                | Some v -> v
                                | None -> []
                            v, keyCache |> Map.add key (v :: values), fkeys
                        | No, Text s, _ -> sGen s, keyCache, fkeys
                        | No, Date, _ -> dtGen(), keyCache, fkeys
                        | No, DateTime, _ -> dttGen(), keyCache, fkeys
                        | No, Decimal(m, s), _ -> dGen m s, keyCache, fkeys
                        | No, Float m, _ -> fGen m, keyCache, fkeys
                        | No, Integer i, _ -> iGen i, keyCache, fkeys
                        | No, Time, _ -> tGen(), keyCache, fkeys
                        | No, TimeStamp, _ -> dttGen(), keyCache, fkeys
                        | No, Unknown, _ -> "null", keyCache, fkeys
                    construct ((name |> sprintf "`%s`", value |> sprintf "%s") :: fields) b newCache newFks

            let values, kCache = construct [] c keyCache foreign

            let insert =
                values
                |> List.unzip
                |> (fun f (v1, v2) -> (f v1, f v2)) (String.concat ", ")
                ||> sprintf "INSERT INTO %s (%s) VALUES (%s);" tab
            insert, kCache

        let foreignKeys =
            c
            |> List.filter (fun c -> c.IsForeign)
            |> List.map (fun c ->
                   match keyCache |> Map.tryFind c.GetForeign with
                   | Some s -> s
                   | None -> [])
            |> List.rev
            |> permute [ [] ]

        match foreignKeys with
        | [] ->
            let insert, keyCache = constructFields [] keyCache
            [ insert ], keyCache
        | fKeys ->
            fKeys |> List.fold (fun (ins, key) fkey ->
                         let insert, kNew = constructFields fkey key
                         (insert :: ins, kNew)) ([], keyCache)