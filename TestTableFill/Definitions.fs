module Definitions
type Foreign =
    | Yes of (string*string)
    | No
type Primary =
    | Yes of (unit->string)
    | No

type DataTypes =
    | Text of int 
    | Integer of int 
    | Float of int
    | Decimal of int*int 
    | Date | Time | TimeStamp | DateTime
    | Unknown

type Column = {Table:string;Name:string;Type:DataTypes;Primary:Primary;Foreign:Foreign;Nullable:bool}
type Table = {Name:string;Columns:Column list}
