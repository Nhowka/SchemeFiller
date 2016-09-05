module ForeignCreator
open Definitions

let create  insertCreator tables =
    let namedMap =
        tables |> List.map (fun t -> t.Name,t) |> Map.ofSeq
    let rec innerCreate (ins, key) tab =
        let dIns, dKey =
            tab.Columns
            |> List.filter (fun c-> c.IsForeign)
            |> List.map (fun c->c.GetForeign |> fst) |> Set.ofList
            |> List.fold innerCreate (ins,key)        
        let nIns, nKey = insertCreator tab dKey
        nIns::dIns,nKey

        

            
