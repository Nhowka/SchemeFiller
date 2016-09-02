module SchemeLoader
open Definitions
open MySql.Data.MySqlClient

let load host port user password scheme =
    let connectionString = sprintf "Server=%s;Port=%i;Database=%s;Uid=%s;Pwd=%s;" host port scheme user password
    use conn = new MySqlConnection(connectionString)
    sprintf "Opening connection to database (%s)." host  |> stderr.WriteLine  
    conn.Open()
    sprintf "Reading tables from scheme (%s)." scheme |> stderr.WriteLine 
    let tables =
      [
        use comm = conn.CreateCommand()
        let tableQuery = sprintf """select TABLE_NAME from information_schema.TABLES where TABLE_SCHEMA='%s' and TABLE_TYPE = 'BASE TABLE';""" scheme
        comm.CommandText <- tableQuery
        use rdr = comm.ExecuteReader()        
        while rdr.Read() do
            yield rdr.GetString(0)]
    tables |> List.map
            (fun table ->  
                sprintf "Reading columns from %s." table |> stderr.WriteLine     
                let columnQuery = sprintf """SELECT DISTINCTROW c.COLUMN_NAME,c.DATA_TYPE, c.character_maximum_length, c.numeric_precision,c.numeric_scale, c.is_nullable
                                               ,CASE WHEN pk.COLUMN_NAME IS NOT NULL THEN 'YES' ELSE 'NO' END AS 'Primary'
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
                                  WHERE c.TABLE_SCHEMA = '%s' AND c.TABLE_NAME = '%s'""" scheme table          
                {Name=table;Columns=[
                        use comm = conn.CreateCommand()
                        
                        comm.CommandText<-columnQuery
                        use rdr = comm.ExecuteReader()
                        while rdr.Read() do
                            let name =  rdr.GetString(0)
                            let dt =
                                match rdr.GetString(1) with
                                |"blob"|"char"|"enum"|"longtext"|"longblob"|"mediumtext"|"mediumblob"|"set"|"text"|"varchar" -> Text (rdr.GetInt32(2))
                                |"bigint"|"int"|"smallint"|"tinyint" -> Integer (rdr.GetInt32(3))
                                |"decimal" -> Decimal (rdr.GetInt32(3),rdr.GetInt32(4))
                                |"double"|"float" -> Float (rdr.GetInt32(3))
                                |"date" -> Date
                                |"time" -> Time
                                |"timestamp" -> TimeStamp
                                |"datetime" -> DateTime
                                |_ -> Unknown
                            let primary =
                                match rdr.GetString(6) with
                                |"YES" -> true
                                |_ -> false
                            let nullable =
                                match rdr.GetString(5) with
                                |"YES" -> true
                                |_ -> false
                            yield (name,dt,primary,nullable)
                    
                    ]|> List.map
                        (fun (name,dt,primary,nullable) ->
                            match primary with
                            |false ->   (name, dt,Primary.No,nullable)
                            |true ->
                                sprintf "Getting value for primary key (%s)." name |> stderr.WriteLine 
                                use comm = conn.CreateCommand()
                                let query = sprintf """SELECT COALESCE(MAX(%s),0) FROM %s""" name table
                                comm.CommandText<-query
                                use rdr = comm.ExecuteReader()
                                if rdr.Read() then
                                    (name,dt,Primary.Yes (Generators.sequentialGenerator (rdr.GetInt32(0))),nullable)
                                else
                                    (name, dt,Primary.No,nullable)
                   )|> List.map
                        (fun (name, dt, primary, nullable)->
                            let query= sprintf """SELECT RC.REFERENCED_TABLE_NAME AS REFERENCED_TABLE_NAME,
                             KCU1.REFERENCED_COLUMN_NAME AS FK_CONSTRAINT_SCHEMA
                             FROM INFORMATION_SCHEMA.REFERENTIAL_CONSTRAINTS AS RC
                             INNER JOIN INFORMATION_SCHEMA.KEY_COLUMN_USAGE AS KCU1 ON KCU1.CONSTRAINT_CATALOG = RC.CONSTRAINT_CATALOG
                             AND KCU1.CONSTRAINT_SCHEMA = RC.CONSTRAINT_SCHEMA AND KCU1.CONSTRAINT_NAME = RC.CONSTRAINT_NAME
                             WHERE RC.TABLE_NAME = '%s' and KCU1.COLUMN_NAME = '%s'""" table name
                            use comm = conn.CreateCommand()
                            comm.CommandText<- query
                            use rdr = comm.ExecuteReader()
                            if rdr.Read() then
                                {Name=name;Type=dt;Primary=primary;Foreign=Foreign.Yes(rdr.GetString(0),rdr.GetString(1));Nullable=nullable}
                            else
                                {Name=name;Type=dt;Primary=primary;Foreign=Foreign.No;Nullable=nullable}
                   )
                       }
                )
    
            
    
    
                    

    

