module ForeignCreator
open Definitions

let create insertCreator n tables =
    let namedMap =
        tables |> List.map (fun t -> t.Name,t) |> Map.ofSeq
    let rec innerCreate (ins, key) tab =
        let dIns, dKey =
            tab.Columns
            |> List.filter (fun c-> c.IsForeign)
            |> List.map (fun c->c.GetForeign |> fst)
            |> List.filter (fun c -> key|> Map.exists (fun (k,_) _ -> k=c) |> not)
            |> Set.ofList
            |> Seq.map (fun n -> namedMap |> Map.find n)
            |> Seq.fold innerCreate (ins,key)
        let nIns, nKey = insertCreator tab dKey
        nIns::dIns,nKey
    tables
    |> List.replicate n
    |> List.collect id
    |> List.fold innerCreate ([], Map.empty)
    |> fst
    |> List.collect id