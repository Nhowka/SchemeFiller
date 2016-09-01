// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open FSharp.Data.Sql.Providers.MySql
open FSharp.Data.Sql.Schema
open FSharp.Data.Sql.Common
open System.Data
open System
open Microsoft.FSharp.Reflection.FSharpReflectionExtensions
open System.Collections.Generic

let namelist = """Hahn
Jaquelyn
Herrera
Nita
Isabelle
Rios
Bartlett
Cantrell
Barrett
Curtis
Helen
Vance
Garza
Michelle
Nola
Fitzgerald
Nora
Buchanan
Cara
Hoyt
Basil
Whitney
Vaughn
Pitts
Caldwell
Jones
Alma
Wade
Joseph
Brianna
Fry
Arthur
Schultz
Ava
Brooke
Mendoza
Sanford
Quyn
Yates
Kellie
Russell
Zeph
Ewing
Booth
Theodore
Ross
Carly
Ora
Moon
Barrett
Duncan
Briggs
Lael
Roach
Dean
Bruce
Wiley
Ria
Ezekiel
Hayfa
Tasha
Perry
Iona
Henry
Buckminster
Alexis
Lilah
Hogan
Klein
Branden
Wolf
Bender
Ferguson
Ramos
Osborne
Cherokee
Meyer
Octavia
Salinas
Jakeem
Rosales
Nieves
Sawyer
Kenyon
Hampton
Xantha
Shana
Flavia
Daniel
Mcdowell
Anastasia
Morin
Mercado
Amethyst
Allistair
Travis
Ava
Karly
Ballard
Anderson"""
let names = namelist.Split([| '\n'; '\r' |], StringSplitOptions.RemoveEmptyEntries) |> Array.distinct

let combine f1 f2 = 
    fun e -> 
        e
        |> f1
        |> f2

let (<+>) = combine

let randomFromPatternGenerator strings pattern = 
    let r = Random()
    
    let cgen = 
        let letterBase = int 'A'
        fun a -> 
            a + match r.Next(52) with
                | n when n > 25 -> char (letterBase + 32 + (n % 26)) |> string
                | n -> char (letterBase + n) |> string
    
    let dgen = fun a -> a + (r.Next(10) |> string)
    
    let sgen = 
        let s = strings |> Seq.toArray
        let l = s.Length
        fun a -> a + s.[r.Next(l)]
    
    let pgen = 
        pattern
        |> Seq.map (function 
               | 'c' -> cgen
               | 'd' -> dgen
               | 's' -> sgen
               | n -> fun e -> e + (string n))
        |> Seq.reduce combine
    
    fun () -> pgen ""

let randomWithNames = randomFromPatternGenerator names

type Data = FSharp.Data.Sql.SqlDataProvider< DatabaseVendor=DatabaseProviderTypes.MYSQL, ConnectionString="Server=127.0.0.1;Database=consinco;Uid=root;Pwd=;", UseOptionTypes=true >

type Primary = 
    | No
    | Yes of (unit -> int)

type SimplifiedMapping = 
    { Type : string
      Size : int
      Scale : int
      Primary : Primary
      Nullable : bool }

let seqGen n = 
    let mutable s = n
    fun _ -> 
        s <- s + 1
        s

let getColumns schema table = 
    Map.ofList [ let conn = Data.GetDataContext().CreateConnection()
                 conn.Open()
                 let comm = conn.CreateCommand()
                 comm.CommandText <- sprintf @"SELECT DISTINCTROW c.COLUMN_NAME,c.DATA_TYPE, Coalesce(c.character_maximum_length, c.numeric_precision,0),Coalesce(c.numeric_scale,0), c.is_nullable
                                               ,CASE WHEN pk.COLUMN_NAME IS NOT NULL THEN 'PRIMARY KEY' ELSE '' END AS KeyType
                                  FROM INFORMATION_SCHEMA.COLUMNS c
                                  LEFT JOIN (
                                              SELECT ku.TABLE_CATALOG,ku.TABLE_SCHEMA,ku.TABLE_NAME,ku.COLUMN_NAME
                                              FROM INFORMATION_SCHEMA.TABLE_CONSTRAINTS AS tc
                                              INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS ku
                                                  ON tc.CONSTRAINT_TYPE = 'PRIMARY KEY'
                                                  AND tc.CONSTRAINT_NAME = ku.CONSTRAINT_NAME
                                           )   pk
                                  ON  c.TABLE_CATALOG = pk.TABLE_CATALOG
                                              AND c.TABLE_SCHEMA = pk.TABLE_SCHEMA
                                              AND c.TABLE_NAME = pk.TABLE_NAME
                                              AND c.COLUMN_NAME = pk.COLUMN_NAME
                                  WHERE c.TABLE_SCHEMA = '%s' AND c.TABLE_NAME = '%s'" schema table
                 let reader = comm.ExecuteReader()
                 while reader.Read() do
                     let dt = reader.GetString(1)
                     match findDbType dt with
                     | Some(m) -> 
                         let col = 
                             { Column.Name = reader.GetString(0)
                               TypeMapping = m
                               IsNullable = 
                                   let b = reader.GetString(4)
                                   if b = "YES" then true
                                   else false
                               IsPrimaryKey = 
                                   if reader.GetString(5) = "PRIMARY KEY" then true
                                   else false }
                         yield (col.Name, 
                                { Type = col.TypeMapping.ClrType
                                  Size = reader.GetInt32(2)
                                  Scale = reader.GetInt32(3)
                                  Primary = 
                                      if col.IsPrimaryKey then 
                                          use connection = Data.GetDataContext().CreateConnection()
                                          connection.Open()
                                          use comm = connection.CreateCommand()
                                          comm.CommandText <- sprintf "SELECT MAX(%s) FROM %s" col.Name table
                                          use rdr = comm.ExecuteReader()
                                          if rdr.Read() then Yes(seqGen (rdr.GetInt32(0)))
                                          else Yes(seqGen 0)
                                      else No
                                  Nullable = col.IsNullable })
                     | _ -> () ]

let decGen = 
    let n = (String.replicate 10 "d" |> randomWithNames)
    fun i s -> 
        let max d = Seq.init d (fun _ -> 10m) |> Seq.fold (*) 1m
        let gen = n() |> decimal
        (gen % (max i)) / (max s)

let intGen = 
    let n = (String.replicate 8 "d" |> randomWithNames)
    fun i -> 
        let max = Seq.init i (fun _ -> 10) |> Seq.reduce (*)
        let gen = n() |> int
        gen % max

let dateGen = randomWithNames "2016-09-0d"
let genName = (randomWithNames "s")

let populate cl schema table filter num = 
    let mapping = getColumns schema table
    
    let entries = 
        if filter then mapping |> Map.filter (fun _ m -> not m.Nullable)
        else mapping
    
    let r = Random()
    for _ in Seq.init num id do
        entries
        |> Map.map (fun _ e -> 
               match e with
               | { Primary = Yes t } -> t() |> box
               | { Type = "System.Decimal"; Size = l; Scale = s } -> decGen l s :> _
               | { Type = "System.UInt32"; Size = l } -> intGen l :> _
               | { Type = "System.String"; Size = l } -> 
                   genName()
                   |> Seq.truncate l
                   |> Array.ofSeq
                   |> String :> _
               | { Type = "System.DateTime" } -> dateGen() :> _
               | { Type = "System.Boolean" } -> r.Next(1) = 0 :> _
               | e -> e :> _)
        |> Map.toSeq
        |> cl

[<EntryPoint>]
let main argv = 
    let context = Data.GetDataContext()
    populate (context.Consinco.MadCargarecprod.Create >> ignore) "consinco" "mad_cargarecprod" false 100
    context.SubmitUpdates()
    //Seq.init 100 (fun _ -> telGenerator()) |> Seq.iter (printfn "%A")
    //getColumns "consinco" "mad_cargareceb" |> printfn "%A"
    //stdin.Read() |> ignore
    0 // return an integer exit code
