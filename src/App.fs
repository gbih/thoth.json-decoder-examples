module ThothExamples

(**
### From Tests to Working Code
* Read the official tests and understand them
* Since the tests should be exhaustive, work-through / identify the range of possible cases
* Adapt useful examples into usable code-snippets
* Base your code upon the tests, referencing the naming, etc. 
*)

// open Fable.Core.JsInterop // !^
// open Fetch
// open Fable.Core


//-------------------------------

module Print =
    let elementId = "elmish-app"
    let elem = Browser.Dom.document.getElementById(elementId)
    elem.setAttribute("style", "color:black; margin:1rem; display: block;font-family: monospace;white-space: pre-wrap;"; )
    
    let p input =
        let x = input
        //let showElement = Browser.Dom.document.createElement("li")
        let showElement = Browser.Dom.document.createElement("span")
        // showElement.innerHTML <- sprintf "%A\n- - - - - - - - - - - - - - - - - - -" x
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

let log = Print.p

let HR () = "- - - - - - - - - - - - - - - - - - -" |> log
let SECTION() = "= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =" |> log 
//-------------------------------

module Random =
    let rand = System.Random()
    let int n = rand.Next(n)
    let float n = n * rand.NextDouble()
    let string n = System.String(Array.init n (fun _ -> char (rand.Next(97,123))))
    let bool = rand.NextDouble() >= 0.5

//---------------------

SECTION()

"Primitives" |> log

SECTION()

module ``a string works`` =
    open Thoth.Json
    let expected = Ok("maxime") 
    let actual =
        Decode.fromString Decode.string "\"maxime\""
``a string works``.actual |> sprintf "a string works\n%A" |> log

// style 1
``a string works``.actual |>
    function
    | Ok values -> values |> log
    | Error error -> failwith error

HR ()

module ``a float works`` =
    open Thoth.Json 
    let expected = Ok(1.2)
    let actual =
        Decode.fromString Decode.float "1.2"
``a float works``.actual |> sprintf "a float works\n%A" |> log

// style 2
let a = ``a float works``.actual
match a with
| Ok values -> values |> log
| Error error -> failwith error

HR ()

module ``a float from int works`` =
    open Thoth.Json
    let expected = Ok 1.0
    let actual =
        Decode.fromString  Decode.float "1.0"
``a float from int works``.actual |> sprintf "a float from int works\n%A" |> log

``a float from int works``.actual |>
    function
    | Ok values -> values |> log
    | Error er -> failwith er

HR ()

module ``a bool works`` =
    open Thoth.Json
    let expected = Ok true
    let actual =
        Decode.fromString Decode.bool "true"

``a bool works``.actual |> log

``a bool works``.actual 
    |> function
        | Ok values -> values |> log
        | Error er -> failwith er

HR ()

module ``an int works`` =
    open Thoth.Json
    let expected = Ok 25
    let actual = 
        Decode.fromString Decode.int "25"

``an int works``.actual |> log

``an int works``.actual |> 
function
| Ok values -> values |> log
| Error x -> failwith x

HR ()


module ``an int64 works from number`` =
    open Thoth.Json
    let expected = Ok 1000L
    let actual =
        Decode.fromString Decode.int64 "1000"

``an int64 works from number``.actual |> log
``an int64 works from number``.actual |>
function
| Ok values -> values |> log
| Error eg -> failwith eg

HR ()

module ``an int64 works from a string`` =
    open Thoth.Json
    let expected = Ok 99L
    let actual =
        Decode.fromString Decode.int64 "\"99\""

``an int64 works from a string``.actual |> log  
``an int64 works from a string``.actual |>
function
| Ok values -> values |> log
| Error er -> failwith er

HR ()

module ``an uint32 works from number`` =
    open Thoth.Json
    let expected = Ok 1000u
    let actual = 
        Decode.fromString Decode.uint32 "1000"
``an uint32 works from number``.actual |> log
``an uint32 works from number``.actual |>
function
| Ok values -> values |> log
| Error er -> failwith er

HR ()

module ``an uint32 works from a string`` =
    open Thoth.Json
    let expected = Ok 1000u
    let actual =
        Decode.fromString Decode.uint32 "\"1000\""

``an uint32 works from a string``.actual |> log

HR ()

module ``a string representing a DateTime should be accepted as a string`` =
    open Thoth.Json
    let expected = "2018-10-01T11:12:55.00Z"
    let actual = 
        Decode.fromString Decode.string "\"2018-10-01T11:12:55.00Z\""

``a string representing a DateTime should be accepted as a string``.actual |> log

HR ()

module ``a datetime works`` =
    open Thoth.Json
    open System
    let expected = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc)
    let actual =
        Decode.fromString Decode.datetime "\"2018-10-01T11:12:55.00Z\""

``a datetime works``.actual |> log

HR ()

module ``a datetime works with TimeZone`` =
    open Thoth.Json
    open System
    let localDate = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Local)

    //let expected = Ok (localDate.ToUniversalTime()) // NOT WORKING
    let json = sprintf "\"%s\"" (localDate.ToString("O"))
    let actual =
        Decode.fromString Decode.datetime json

    localDate.ToUniversalTime() |> log

HR ()

module ``a datetimeOffset works`` =
    open Thoth.Json
    open System
    let expected = DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
    let json = "\"2018-07-02T12:23:45+02:00\""
    let actual =
        Decode.fromString Decode.datetimeOffset json

``a datetimeOffset works``.actual |> log

HR ()

module ``a timespan works`` =
    open Thoth.Json
    open System
    let expected =
        TimeSpan(23, 45, 0)
    let json = "\"23:45:00\""
    let actual =
        Decode.fromString Decode.timespan json

``a timespan works``.actual |> log

SECTION()

"Tuples" |> log

SECTION()

module ``tuple2 works`` =
    open Thoth.Json
    let json = """[1, "maxime"]"""
    let expected = Ok(1, "maxime")
    let actual = 
        Decode.fromString (Decode.tuple2 Decode.int Decode.string) json
``tuple2 works``.actual |> log

``tuple2 works``.actual |>
function
| Ok values -> values |> log
| Error err -> failwith err |> log

HR ()

module ``tuple3 works`` =
    open Thoth.Json
    let json = """[1, "maxime", 2.5]"""
    let expected = Ok(1, "maxime", 2.5)
    let actual =
        Decode.fromString
            (Decode.tuple3
                Decode.int
                Decode.string
                Decode.float
            ) json
``tuple3 works``.actual |> log

// style 1
let tuple3works = ``tuple3 works``.actual
match tuple3works with
| Ok values -> values |> log
| Error err -> failwith err |> log

// style 2
match ``tuple3 works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

// style 3
``tuple3 works``.actual |>
function
| Ok values -> values |> log
| Error err -> failwith err |> log


//----------

// Continue for tuple3 .. tuple8

//----------

SECTION()

"Object primitives" |> log

SECTION()

module ``field works`` =
    open Thoth.Json
    let json = """{ "name": "maxime", "age":25}"""
    let expected = Ok(25)
    let actual =
        Decode.fromString (Decode.field "age" Decode.int) json
    let actual2 =
        Decode.fromString (Decode.field "height" Decode.int) json

``field works``.actual |> log
``field works``.actual = ``field works``.expected |> log

match ``field works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``index works`` =
    open Thoth.Json
    let json = """["maxime", "alfonso", "steffen"]"""
    let expected = Ok("alfonso")
    let actual =
        Decode.fromString (Decode.index 1 Decode.string) json
``index works``.actual |> log
``index works``.actual = ``index works``.expected |> log
match ``index works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

SECTION()

"Data structure" |> log

SECTION()










// // non-function
// module Decoder001 =
//     open Thoth.Json
//     let actual = Decode.fromString Decode.string "\"maxime\""
//     actual |> sprintf "Decode.fromString:\n%A" |> log

//     match actual with
//     | Ok values ->
//         values |> log
//     | Error values -> 
//         values |> log


// module Decoder001b =
//     open Thoth.Json
//     let actual =
//         let x = Decode.fromString Decode.string "\"maxime\""
//         match x with
//             | Ok values -> 
//                 values
//             | Error values ->
//                 "Error"
// Decoder001b.actual |> sprintf "module with function and destructuring of `Ok Error` case: \n%A"  |> log


// module Decoder001c =
//     open Thoth.Json
//     let actual =
//         Decode.fromString Decode.string "\"maxime\""
//         |> function
//         | Ok values ->
//             values
//         | Error values ->
//             "Error"
// Decoder001c.actual |> sprintf "module with function, more streamlined handling of `Ok Error` case: %A" |> log


// module Decoder001d =
//     open Thoth.Json
//     let json = """{ "name" : "maxime", "age": 25 }"""
//     let actual = Decode.fromString (Decode.field "age" Decode.int ) json
//     actual |> sprintf "Object primitives - Decode.fromString (Decode.field):\n%A" |> log

//     match actual with
//     | Ok values ->
//         values |> log
        
//     | Error values -> 
//         values |> log


// module Decoder001e =
//     open Thoth.Json

//     let json = 
//         """
// [1, "maxime"]
//         """
        
//     let actual json = 
//         let test = Decode.fromString (Decode.tuple2 Decode.int Decode.string) json        
//         match test with
//         | Ok value -> value
//         | Error er -> failwith er

//     actual json |> sprintf "Tuple example, pattern on `Ok Error case`:\n%A" |> log



// module Decoder002 =
//     open Thoth.Json 
//     let actual  =
//         //Decode.unsafeFromString Decode.string "\"maxime\""
//         Decode.fromString Decode.string "\"maxime\""
//     actual |> sprintf "Decode.unsafeFromString:\n%A" |> log

