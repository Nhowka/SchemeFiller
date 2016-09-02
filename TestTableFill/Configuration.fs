module Configuration
open System.IO
type Config =
    { 
        ShowHelp:bool
        WrongArgument:string option
        StringList:string option
        Host:string
        Port:int
        Username:string
        Password:string
        Database:string option
    }
    static member private help = "Usage: SchemeFiller [-h|--help|/?] [-s|--StringList stringList.txt] [-h|--Host host] [-p|--Port port] [-u|--User user] [-P|--Password password] [-d|--Database database] 

-h | --help | /? \t\t Show this menu
-s | --StringList\t\t Set the input file for the strings used to fill the tables
-h | --Host      \t\t Choose the host address (default 127.0.0.1)
-p | --Port      \t\t Choose the host port (default 3306)
-u | --User      \t\t Choose the username (default root)
-P | --Password  \t\t Choose the password (default BLANK)
-d | --Database  \t\t Choose the schema to be filled

The string list will is expected to be a text file with one string per line. If not set a list will be generated with strings with 5 to 10 random alpha-characters."
    static member defaultConfig =
            { 
               ShowHelp=false
               WrongArgument=None
               StringList=None
               Host="127.0.0.1"
               Port=3306
               Username="root"
               Password=""
               Database=None
            }
    static member private ParseArgs args =
            let rec parse config =
                function
                |[] -> config
                |"/?"::_|"-h"::_|"--help"::_ -> {config with ShowHelp=true}
                |"-s"::s::a|"--StringList"::s::a -> parse {config with StringList=Some s} a
                |"-d"::d::a|"--Database"::d::a -> parse {config with Database=Some d} a
                |"-u"::u::a|"--Username"::u::a -> parse {config with Username=u} a
                |"-h"::h::a|"--Host"::h::a -> parse {config with Host=h} a
                |"-p"::p::a|"--Port"::p::a -> parse {config with Port=int p} a
                |"-P"::p::a|"--Password"::p::a -> parse {config with Password=p} a                
                |a::_ -> {config with WrongArgument=Some a}
            (args |> Seq.toList) |> parse Config.defaultConfig
    static member Run args =
        match Config.ParseArgs args with
        |{WrongArgument=Some a} -> sprintf "Wrong argument: %s\nUse /?, -h or --help to see a list of supported commands." a |> stderr.Write
        |{ShowHelp=true} -> stderr.Write Config.help
        |{Database=None} -> stderr.Write "You need to set a database/schema. Use -h for see usage example."
        |{Host=h;Port=p;Username=u;Password=pwd;Database=Some d;StringList=s} ->
            let creator =
                match s with
                |Some f -> File.ReadAllLines(f)
                |None -> [||]
                |> InsertCreator.insertCreator
            SchemeLoader.load h p u pwd d
            |>
            Seq.iter (creator>>(printfn "%s"))
        