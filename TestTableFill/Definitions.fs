module Definitions
type Foreign = 
    | Yes of (string * string)
    | No

type Primary = 
    | Yes of (unit -> string)
    | No

type DataTypes = 
    | Text of int
    | Integer of int
    | Float of int
    | Decimal of int * int
    | Date
    | Time
    | TimeStamp
    | DateTime
    | Unknown

type Column = 
    { Table : string
      Name : string
      Type : DataTypes
      Primary : Primary
      Foreign : Foreign
      Nullable : bool }

      member this.IsForeign =
        match this.Foreign with
        |Foreign.Yes _ -> true
        |Foreign.No -> false
      member this.GetForeign =
        match this.Foreign with
        |Foreign.Yes f -> f
        |Foreign.No -> invalidOp "Column is not a reference"
      member this.IsPrimary =
        match this.Primary with
        |Primary.Yes _ -> true
        |Primary.No -> false

type Table = 
    { Name : string
      Columns : Column list }
      
