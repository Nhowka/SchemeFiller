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
        let rec innerCreator x (dIns,dKey) =
            match x with
            |0 -> dIns,dKey
            |_ -> 
                let nIns,nKey =insertCreator tab dKey
                (nIns::dIns,nKey)|> innerCreator (x-1)
        innerCreator n (ins, key)
    tables    
    |> List.fold innerCreate ([], Map.empty)
    |> fst
    |> List.collect id