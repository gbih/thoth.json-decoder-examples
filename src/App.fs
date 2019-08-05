module ThothEncoderExamples
open Tests.Types

(**
This library is based on the Elm JSON decoder/encoder libraries:

https://guide.elm-lang.org/effects/json.html
http://package.elm-lang.org/packages/elm-lang/core/latest/Json-Decode
https://package.elm-lang.org/packages/NoRedInk/elm-decode-pipeline/latest
*)

(**
Note for module names:
http://fsharp.org/specs/language-spec/3.0/FSharpSpec-3.0-final.pdf
Any sequence of characters that is enclosed in double-backtick marks (``   ``), 
excluding newlines,tabs , and double-back tick pairs themselves, is treated as 
an identifier. Note that when an identifier is used for the name of a type, 
union type case, module, or namespace, the following characters are not allowed 
even inside double-backtick marks:
‘.', '+', '$', '&', '[', ']', '/', '\\', '*', '\"', '`'
*)

module Print =
    open Fable.Core

    [<Emit("JSON.stringify($0,null,3)")>]
    let JSONStringify x : 'a = jsNative

    let elementId = "elmish-app"
    let elem = Browser.Dom.document.getElementById(elementId)
    elem.setAttribute("style", "color:black; margin:1rem; display: block;font-family: monospace;font-size:1.1rem;white-space: pre-wrap;"; )
    
    let p input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

    let title input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.setAttribute("style", "margin-bottom:1rem;font-style:italic;color:blue"; )
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

    let json input =
        let x = input
        let showElement = Browser.Dom.document.createElement("span")
        showElement.setAttribute("style", "margin-bottom:1rem;font-style:italic;color:green"; )
        showElement.innerHTML <- sprintf "%A\n" x
        Browser.Dom.document.getElementById(elementId).appendChild showElement |> ignore

let log = Print.p
let logtitle = Print.title
let logjson = Print.json

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

let jsonRecord =
    """{ "a": 1.0,
         "b": 2.0,
         "c": 3.0,
         "d": 4.0,
         "e": 5.0,
         "f": 6.0,
         "g": 7.0,
         "h": 8.0 }"""

let jsonRecordInvalid =
    """{ "a": "invalid_a_field",
         "b": "invalid_a_field",
         "c": "invalid_a_field",
         "d": "invalid_a_field",
         "e": "invalid_a_field",
         "f": "invalid_a_field",
         "g": "invalid_a_field",
         "h": "invalid_a_field" }"""

//-------------------------------
// Primitives
//-------------------------------

SECTION()

"Primitives" |> logtitle

SECTION()

module ``a string works`` =
    open Thoth.Json
    let expected = Ok("maxime") 
    let actual =
        Decode.fromString Decode.string "\"maxime\""

"module ``a string works``" |> logtitle
``a string works``.actual |> sprintf "a string works\n%A" |> log

"after pattern matching on Ok Error" |> logtitle
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

"module ``a float works``" |> logtitle
``a float works``.actual |> sprintf "a float works\n%A" |> log

"after pattern matching on Ok Error" |> logtitle
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

"module ``a float from int works``" |> logtitle
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

"module ``a bool works``" |> logtitle
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

"module ``an int works``" |> logtitle
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

"module ``an int64 works from number``" |> logtitle
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

"module ``an int64 works from a string``" |> logtitle
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

"module ``an uint32 works from number``" |> logtitle
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

"module ``an uint32 works from a string``" |> logtitle
``an uint32 works from a string``.actual |> log

HR ()

module ``a string representing a DateTime should be accepted as a string`` =
    open Thoth.Json
    let expected = "2018-10-01T11:12:55.00Z"
    let actual = 
        Decode.fromString Decode.string "\"2018-10-01T11:12:55.00Z\""

"module ``a string representing a DateTime should be accepted as a string``" |> logtitle
``a string representing a DateTime should be accepted as a string``.actual |> log

HR ()

module ``a datetime works`` =
    open Thoth.Json
    open System
    let expected = DateTime(2018, 10, 1, 11, 12, 55, DateTimeKind.Utc)
    let actual =
        Decode.fromString Decode.datetime "\"2018-10-01T11:12:55.00Z\""

"module ``a datetime works``" |> logtitle
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

"module ``a datetime works with TimeZone``" |> logtitle
``a datetime works with TimeZone``.localDate.ToUniversalTime() |> log

HR ()

module ``a datetimeOffset works`` =
    open Thoth.Json
    open System
    let expected = DateTimeOffset(2018, 7, 2, 12, 23, 45, 0, TimeSpan.FromHours(2.))
    let json = "\"2018-07-02T12:23:45+02:00\""
    let actual =
        Decode.fromString Decode.datetimeOffset json

"module ``a datetimeOffset works``" |> logtitle
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

"module ``a timespan works``" |> logtitle
``a timespan works``.actual |> log

//-------------------------------
// Tuples
//-------------------------------

SECTION()

"Tuples" |> log

SECTION()

module ``tuple2 works`` =
    open Thoth.Json
    let json = """[1, "maxime"]"""
    let expected = Ok(1, "maxime")
    let actual = 
        Decode.fromString (Decode.tuple2 Decode.int Decode.string) json

"module ``tuple2 works``" |> logtitle
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

"module ``tuple3 works``" |> logtitle
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


// To-do: Continue for tuple3 .. tuple8

//-------------------------------
// Object Primitives
//-------------------------------

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

"module ``field works``" |> logtitle
``field works``.actual |> log
``field works``.actual = ``field works``.expected |> log

match ``field works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``index works`` =
    open Thoth.Json
    let json = """["maxime", "alfonso", "steffen"]"""
    let expected = Ok("steffen")
    let actual =
        Decode.fromString (Decode.index 2 Decode.string) json

"module ``index works``" |> logtitle
``index works``.actual |> log
``index works``.actual = ``index works``.expected |> log
match ``index works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

//-------------------------------
// Data strucure: List
//-------------------------------

SECTION()

"Data structure: List" |> log

SECTION()

module ``list works`` =
    open Thoth.Json
    let expected = Ok([1; 2; 3])
    let actual =
        Decode.fromString (Decode.list Decode.int) "[1, 2, 3]"

"module ``list works``" |> logtitle
``list works``.actual |> log
``list works``.actual = ``list works``.expected |> log
match ``list works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``nested lists work`` =
    open Thoth.Json
    let expected = Ok([[ "maxime" ]])
    let actual =
        [[ "maxime" ]]
        |> List.map (fun d ->
            d
            |> List.map Encode.string
            |> Encode.list)
        |> Encode.list
        |> Encode.toString 4
        |> Decode.fromString (Decode.list (Decode.list Decode.string))

"module ``nested lists work``" |> logtitle
``nested lists work``.actual |> log
``nested lists work``.actual = ``nested lists work``.expected |> log

HR()

module ``nested lists work and case handling`` =
    open Thoth.Json
    let expected = Ok([[ "maxime" ]])
    let actual =
        [[ "maxime" ]]
        |> List.map (fun d ->
            d
            |> List.map Encode.string
            |> Encode.list)
        |> Encode.list
        |> Encode.toString 4
        |> Decode.fromString (Decode.list (Decode.list Decode.string))
        |> function
        | Ok v -> v
        | Error err -> failwith err

"module ``nested lists work and case handling``" |> logtitle
``nested lists work and case handling``.actual |> log

HR()

// 1-D list
module ``nested lists work 1D-A`` =
    open Thoth.Json
    let expected = [["maxime"]]
    let actual =
        [ "maxime" ]
        |> List.map (fun d ->
            d
            |> Encode.toString 0
        )

"module ``nested lists work 1D-A``" |> logtitle
``nested lists work 1D-A``.actual |> sprintf "1D Encoding:\n%A" |> log

HR()

module ``nested lists work 2D-1`` =
    open Thoth.Json
    let expected = [["maxime"]]
    let actual =
        [ [ "maxime2" ] ]
            |> List.map (fun d ->
                d
                |> List.map Encode.string
                |> Encode.list)
            |> Encode.list
            |> Encode.toString 4

"module ``nested lists work 2D-1``" |> logtitle
``nested lists work 2D-1``.actual |> sprintf "2D Encoding:\n%A" |> log

HR()


// 1-D list, another variation for encoding
module ``nested lists work 1D-B`` =
    open Thoth.Json
    let expected = ["maxime"]
    let actual =
        Encode.list
            [ Encode.string "maxime"]
        |> Encode.toString 4

"``nested lists work 1D-B``" |> logtitle
``nested lists work 1D-B``.actual |> sprintf "1D Encoding:\n%A" |> log

// HOW TO TO DO 3D LIST ????

SECTION()

"Data structure: Array" |> log

SECTION()

module ``array works`` =
    open Thoth.Json
    // Need to pass by a list otherwise Fable use:
    // new Int32Array([1, 2, 3]) and the test fails
    // And this would give:
    // Expected: Result { tag: 0, data: Int32Array [ 1, 2, 3 ] }
    // Actual: Result { tag: 0, data: [ 1, 2, 3 ] }
    let expected : Result<int [],string> = Ok([1; 2; 3] |> List.toArray)

    let actual = 
        Decode.fromString (Decode.array Decode.int) "[1, 2, 3]"

"module ``array works``" |> logtitle
``array works``.actual |> log
``array works``.actual = ``array works``.expected |> log
``array works``.actual 
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

//-------------------------------
// Data structure: keyValue Pairs
//-------------------------------

SECTION()

"Data structure: keyValue Pairs" |> log

SECTION()

module ``keyValuePairs works`` =
    open Thoth.Json
    let expected = Ok([("a", 1) ; ("b", 2) ; ("c", 3)])
    let actual =
        Decode.fromString (Decode.keyValuePairs Decode.int) """{ "a":1, "b":2, "c":3}"""

"module ``keyValuePairs works``" |> logtitle
``keyValuePairs works``.actual |> log
``keyValuePairs works``.actual = ``keyValuePairs works``.expected |> log
``keyValuePairs works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

//-------------------------------
// Data strucure: dict
//-------------------------------

SECTION()

"Data structure: dict" |> log

SECTION()

module ``dict works`` =
    open Thoth.Json
    let expected : Result<Map<string,int>,string> = Ok(Map.ofList([("a",1); ("b", 2); ("c", 3)]))
    let actual = 
        Decode.fromString (Decode.dict Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

"module ``dict works``" |> logtitle
``dict works``.actual |> log
``dict works``.actual = ``dict works``.expected |> log

``dict works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``dict with custom decoder works`` =
    open Thoth.Json
    // Can be confusing -- what type wraps which types?
    // Result is the "highest"-level type, akin to an Option, wrapping around everything here
    // Next, Map is the second-highest level type, wrapping <string,Record2>
    let expected :  Result<Map<string,Record2>,string> = 
        Ok(Map.ofList(
            [
                ("a", Record2.Create 1. 1.)
                ("b", Record2.Create 2. 2.)
                ("c", Record2.Create 3. 3.)
            ]
        ))

    let decodePoint =
        Decode.map2 Record2.Create
            (Decode.field "a" Decode.float)
            (Decode.field "b" Decode.float)

    let actual = 
        Decode.fromString (Decode.dict decodePoint)
            """
{
    "a":
        {
            "a": 1.0,
            "b": 1.0
        },
    "b":
        {
            "a": 2.0,
            "b": 2.0
        },
    "c":
        {
            "a": 3.0,
            "b": 3.0
        }
}
            """

"module ``dict with custom decoder works``" |> logtitle
``dict with custom decoder works``.actual |> log
``dict with custom decoder works``.actual  = ``dict with custom decoder works``.expected |> log

``dict with custom decoder works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

//-------------------------------
// Inconsistent data structure
//-------------------------------

SECTION()

"Inconsistent structure" |> log

SECTION()

module ``oneOf works`` =
    open Thoth.Json
    let expected = Ok([1;2;0;4])

    let badInt =
        Decode.oneOf [ Decode.int; Decode.nil 0]

    let actual =
        Decode.fromString (Decode.list badInt) "[1,2,null,4]"

"``oneOf works``" |> logtitle
``oneOf works``.expected |> sprintf "expected: %A" |> log
``oneOf works``.actual |> log
``oneOf works``.actual = ``oneOf works``.expected |> log

``oneOf works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``oneOf works in combination with object builders`` =
    open Thoth.Json
    let json = """{ "Bar": { "name": "maxime", "age": 25 } }"""
    let expected = Ok(Choice2Of2 { fieldA = "maxime" })

    let decoder1 =
        Decode.object (fun get ->
            { fieldA = get.Required.Field "name" Decode.string })

    let decoder2 =
        Decode.oneOf [
            Decode.field "Foo" decoder1 |> Decode.map Choice1Of2
            Decode.field "Bar" decoder1 |> Decode.map Choice2Of2
        ]

    let actual = 
        Decode.fromString decoder2 json

"``oneOf works in combination with object builders``" |> logtitle
``oneOf works in combination with object builders``.json |> log
``oneOf works in combination with object builders``.actual |> log

``oneOf works in combination with object builders``.expected
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log


(**
The definition of the Choice type is as follows:

type Choice<'a, 'b> =
    | Choice1Of2 of 'a
    | Choice2Of2 of 'b

https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/core.choice%5B't1%2C't2%5D-union-%5Bfsharp%5D
Core.Choice<'T1,'T2> Union (F#)
Helper types for active patterns with two choices.

Example:
https://gist.github.com/viswaug/f6b3ec4df66f55b1d037

*)

HR()

module ``oneOf works with optional`` =
    open Thoth.Json
    let expected1 = (Ok(Normal 4.5))
    let expected2 = (Ok(Reduced(Some 4.5)))
    let expected3 = (Ok(Reduced None))
    let expected4 = (Ok Zero)

    let decoder =
        Decode.oneOf
            [
                Decode.field "Normal" Decode.float |> Decode.map Normal
                Decode.field "Reduced" (Decode.option Decode.float) |> Decode.map Reduced
                Decode.field "Zero" Decode.bool |> Decode.map (fun _ -> Zero)
            ]
    let actual1 = 
        """{"Normal": 4.5}""" |> Decode.fromString decoder
    let actual2 =
        """{"Reduced": 4.5}""" |> Decode.fromString decoder
    let actual3 =
        """{"Reduced": null}""" |> Decode.fromString decoder
    let actual4 =
        """{"Zero": true}""" |> Decode.fromString decoder 

"module ``oneOf works with optional``" |> logtitle
``oneOf works with optional``.actual1 |> log
``oneOf works with optional``.actual1 = ``oneOf works with optional``.expected1 |> log

``oneOf works with optional``.actual2 |> log
``oneOf works with optional``.actual2 = ``oneOf works with optional``.expected2 |> log

``oneOf works with optional``.actual3 |> log
``oneOf works with optional``.actual3 = ``oneOf works with optional``.expected3 |> log

``oneOf works with optional``.actual4 |> log
``oneOf works with optional``.actual4 = ``oneOf works with optional``.expected4 |> log

HR()

module ``oneOf output errors if all case fails`` =
    open Thoth.Json
    let badInt =
        Decode.oneOf [ Decode.string; Decode.field "test" Decode.string;  ]
    let actual =
        Decode.fromString (Decode.list badInt) "[1,2,null,4]"

"module ``oneOf output errors if all case fails``" |> logtitle
``oneOf output errors if all case fails``.actual |> log

HR()

module ``optional works`` =
    open Thoth.Json
    let json = """{ "name": "maxime", "age": 25, "something_undefined": null }"""

    let expectedValid = Ok(Some "maxime")
    let actualValid =
        Decode.fromString (Decode.optional "name" Decode.string) json

    "module ``optional works``" |> logtitle

    actualValid |> log
    expectedValid = actualValid |> log

    match Decode.fromString (Decode.optional "name" Decode.int) json with
    | Error _ -> ()
    | Ok _ -> failwith "Expected type error for `name` field"

    let expectedMissingField = Ok(None)
    let actualMissingField =
        Decode.fromString (Decode.optional "height" Decode.int) json

    actualMissingField |> log
    expectedMissingField = actualMissingField |> log


    let expectedUndefinedField = Ok(None)
    let actualUndefinedField =
        Decode.fromString (Decode.optional "something_undefined" Decode.string) json

    actualUndefinedField |> log
    expectedUndefinedField = actualUndefinedField |> log


HR()

module ``optionalAt works`` =
    open Thoth.Json
    let json = """{ "data" : { "name": "maxime", "age": 25, "something_undefined": null } }"""

    let expectedValid = Ok(Some "maxime")

    let actualValid =
        Decode.fromString (Decode.optionalAt ["data"; "name"] Decode.string) json

    match Decode.fromString (Decode.optionalAt ["data"; "name"] Decode.int) json with
    | Error _ -> ()
    | Ok _ -> failwith "Expected type error for `name` field"

    let expectedMissingField = Ok None
    let actualMissingField =
        Decode.fromString (Decode.optionalAt [ "data"; "height"] Decode.int) json

    let expectedUndefinedField = Ok(None)
    let actualUndefinedField =
        Decode.fromString (Decode.optionalAt ["data"; "something_undefined"] Decode.string) json

"module ``optionalAt works``" |> logtitle
``optionalAt works``.actualValid |> log
``optionalAt works``.actualMissingField |> log
``optionalAt works``.actualUndefinedField |> log

HR()

module ``combining field and option decoders works`` =
    open Thoth.Json
    let json = """{ "name": "maxime", "age": 25, "something_undefined": null }"""

    let expectedValid = Ok(Some "maxime")
    let actualValid =
        Decode.fromString (Decode.field "name" (Decode.option Decode.string)) json

"module ``combining field and option decoders works``" |> logtitle
``combining field and option decoders works``.actualValid |> log

//-------------------------------
// Fancy decoding
//-------------------------------

SECTION()

"Fancy decoding" |> log

SECTION()

module ``null works (test on an int)`` =
    open Thoth.Json
    let expected = Ok(20)
    let actual =
        Decode.fromString (Decode.nil 20) "null"

"module ``null works (test on an int)``" |> logtitle
``null works (test on an int)``.actual |> log
``null works (test on an int)``.actual = ``null works (test on an int)``.expected |> log

``null works (test on an int)``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log

HR()

module ``null works (test on a boolean)`` =
    open Thoth.Json
    let expected = Ok(false)
    let actual =
        Decode.fromString (Decode.nil false) "null"

"module ``null works (test on a boolean)``" |> logtitle
``null works (test on a boolean)``.actual |> log

// explicit style with lamba and match with
``null works (test on a boolean)``.actual
|> fun x -> 
    match x with
    | Ok values -> values |> log
    | Error err -> failwith err |> log


// shortcut syntax via function
``null works (test on a boolean)``.actual
|> function
    | Ok values -> values |> log
    | Error err -> failwith err |> log

HR()

module ``succeed works`` =
    open Thoth.Json 
    let expected = Ok(7)
    let actual =
        Decode.fromString (Decode.succeed 70) "true"

"module ``succeed works``" |> logtitle
``succeed works``.actual |> log

HR()

// succeed is a Result-type
module ``succeed output an error if the JSON is invalid`` =
    open Thoth.Json 
    let expected = Error("Given an invalid JSON: Unexpected token m in JSON at position 0")
    let actual =
        Decode.fromString (Decode.succeed 7) "maxime"

"module ``succeed output an error if the JSON is invalid``" |> logtitle
``succeed output an error if the JSON is invalid``.actual |> log

HR()

module ``fail works`` =
    open Thoth.Json 
    let msg = "Failing because it's fun"
    let expected : Result<obj, string> = Error("Error at: `$`\nThe following `failure` occurred with the decoder: " + msg)
    let actual : Result<obj, string> =
        Decode.fromString (Decode.fail msg) "true"

"module ``fail works``" |> logtitle
``fail works``.actual |> log
``fail works``.actual = ``fail works``.expected |> log

HR()

module ``andThen works`` =
    open Thoth.Json
    let expected = Ok 1

    let infoHelp version =
        match version with
        | 4 -> 
            Decode.succeed 11
        | 3 ->
            Decode.succeed 12
        | _ -> 
            Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

    let info : Decoder<int> =
        Decode.field "version" Decode.int
        |> Decode.andThen infoHelp

    let actual =
        Decode.fromString info """{ "version": 3, "data": 2 }"""

"module ``andThen works``" |> logtitle
``andThen works``.actual |> log

HR()

module ``andThen generate an error if an error occurred`` =
    open Thoth.Json
    let expected : Result<obj,string> =
        Error(
            """
Error at: `$`
Expecting an object with a field named `version` but instead got:
{
    "info": 3,
    "data": 2
}
            """.Trim())
        
    let infoHelp version : Decoder<int> =
        match version with
        | 4 ->
            Decode.succeed 1
        | 3 ->
            Decode.succeed 1
        | _ -> 
            Decode.fail <| "Trying to decode info, but version " + (version.ToString()) + "is not supported"

    let info =
        Decode.field "version" Decode.int
        |> Decode.andThen infoHelp

    let actual =
        Decode.fromString info """{ "info": 3, "data": 2 }"""
    
"module ``andThen generate an error if an error occurred``" |> logtitle
``andThen generate an error if an error occurred``.actual |> log

//-------------------------------
// Mapping
//-------------------------------

SECTION()

"Mapping" |> log

SECTION()

(**
Map functions:

To get data from several fields and convert them into a record you will 
need to use the map functions like map2, map3, ..., map8
https://mangelmaxime.github.io/Thoth/json/v3.html
https://guide.elm-lang.org/effects/json.html
*)

module ``map example`` =
    open Thoth.Json
    type Point = 
        { X : int }
        static member Decoder : Decoder<Point> =
            Decode.map 
                (fun x  -> { X = x } : Point) // constructor
                (Decode.field "x" Decode.int) // decoder 1

    let actual = Decode.fromString Point.Decoder """{"x": 10} """

"module ``map example``" |> logtitle
``map example``.actual |> log

HR()

// Try two decoders and then combine the result. 
// We can use this to decode objects with many fields.
module ``map2 example`` =
    open Thoth.Json
    type Point =
        { X : int; Y : string}
        static member Decoder : Decoder<Point> =
            Decode.map2 
                (fun x y -> { X = x; Y = y } : Point) // constructor
                (Decode.field "x" Decode.int) // decoder 1
                (Decode.field "y" Decode.string) // decoder 2

    let actual = Decode.fromString Point.Decoder """{"x": 10, "y": "George"}"""

"module ``map2 example``" |> logtitle
``map2 example``.actual |> log

HR()

module ``map works`` =
    open Thoth.Json
    let expected = Ok(6)

    let stringLength : string -> JsonValue -> Result<int,DecoderError> =
        Decode.map 
            String.length // constructor
            Decode.string // decoder 1

    // ctor is constructor
    let actual =
        Decode.fromString stringLength "\"maxime\""

"module ``map works`" |> logtitle
``map works``.actual |> log

HR()

module ``map2 works`` =
    open Thoth.Json
    let expected = Ok({a = 1.; b = 2.} : Record2)

    let decodePoint : string -> JsonValue -> Result<Record2,DecoderError> = 
        Decode.map2 
            Record2.Create // constructor
            (Decode.field "a" Decode.float) // decoder 1
            (Decode.field "b" Decode.float) // decoder 2

    let actual =
        Decode.fromString decodePoint jsonRecord

"module ``map2 works``" |> logtitle
``map2 works``.actual |> log

// To-do: Finish examples for map3..map8

//-------------------------------
// object builder
//-------------------------------

SECTION()

"object builder" |> log

SECTION()

module ``get Required Field works`` =
    open Thoth.Json
    let json = """{"name":"maxime", "age": 25}"""
    let expected = Ok({fieldA = "maxime"})

    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.Field "name" Decode.string}
            )
    
    let actual = 
        Decode.fromString decoder json

"module ``get Required Field works``" |> logtitle
``get Required Field works``.actual |> log

HR()

module ``get Required Field returns Error if field is missing`` =
    open Thoth.Json
    let jsonWithoutTrim = """  { "age": 25 }  """
    let json = """  { "age": 25 }  """.Trim()
    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.Field "name" Decode.string}    
            )

    let actual = 
        Decode.fromString decoder json

"module ``get Required Field returns Error if field is missing``" |> logtitle
``get Required Field returns Error if field is missing``.actual |> log
``get Required Field returns Error if field is missing``.json |> log
``get Required Field returns Error if field is missing``.jsonWithoutTrim |> log

HR()

module ``getRequiredField returns Error if type is incorrect`` =
    open Thoth.Json
    let json = """{ "name": 12, "age": 25 }""".Trim()

    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.Field "name" Decode.string }
            )

    let actual =
        Decode.fromString decoder json

"module ``getRequiredField returns Error if type is incorrect``" |> logtitle
``getRequiredField returns Error if type is incorrect``.actual |> log

HR()

module ``getOptionalField works`` =
    open Thoth.Json
    let json = """{"name":"maxime", "age":25}""".Trim()

    let expected = Ok({optionalField = Some "maxime"})

    let decoder = 
        Decode.object
            (fun get ->
                // same syntax as for getting a field
                { optionalField = get.Optional.Field "name" Decode.string }
            )
    let actual = 
        Decode.fromString decoder json

"module ``getOptionalField works``" |> logtitle
``getOptionalField works``.actual |> log

HR()

module ``getOptionalField returns None value if field is missing`` =
    open Thoth.Json
    let json = """{"age":25}""".Trim()
    let expected = Ok({optionalField = None})

    let decoder =
        Decode.object
            (fun get ->
                {optionalField = get.Optional.Field "name" Decode.string}
            )
    let actual = 
        Decode.fromString decoder json

"module ``getOptionalField returns None value if field is missing``" |> logtitle
``getOptionalField returns None value if field is missing``.actual |> log
``getOptionalField returns None value if field is missing``.actual = ``getOptionalField returns None value if field is missing``.expected |> log

HR()

module ``getOptionalField returns None if field is null`` =
    open Thoth.Json 
    let json = """{ "name": null, "age": 25 }""".Trim()

    let decoder = 
        Decode.object
            (fun get ->
                    { optionalField = get.Optional.Field "name" Decode.string}
            )

    let actual = 
        Decode.fromString decoder json

"module ``getOptionalField returns None if field is null``" |> logtitle
``getOptionalField returns None if field is null``.actual |> log

HR()

module ``getOptionalField returns Error value if decoder fails`` =
    open Thoth.Json
    let json = """{"name":12, "age":25}""".Trim()
    
    let decoder = 
        Decode.object(
            fun get ->
                { optionalField = get.Optional.Field "name" Decode.string }
        )

    let actual =
        Decode.fromString decoder json

"module ``getOptionalField returns Error value if decoder fails``" |> logtitle
``getOptionalField returns Error value if decoder fails``.actual |> log

HR()

module ``nested getOptionalField greaterthan getRequiredField returns None if field is null`` =
    open Thoth.Json
    let json = """{"user":null, "field2":25}""".Trim()
    let expected = Ok({User = None; Field2 = 25})

    let userDecoder =
        Decode.object
            (fun get ->
                {
                    Id = get.Required.Field "id" Decode.int
                    Name = get.Required.Field "name" Decode.string
                    Email = get.Required.Field "email" Decode.string
                    Followers = 0
                }
            )

    let decoder =
        Decode.object
            (fun get ->
                { 
                    User = get.Optional.Field "user" userDecoder
                    Field2 = get.Required.Field "field2" Decode.int 
                }
            )

    let actual =
        Decode.fromString decoder json

"module ``nested getOptionalField greaterthan getRequiredField returns None if field is null``" |> logtitle
``nested getOptionalField greaterthan getRequiredField returns None if field is null``.actual |> log

module ``getOptionalField returns Error if type is incorrect`` =
    open Thoth.Json
    let json = """{ "name": 12, "age": 25 }"""

    let decoder =
        Decode.object
            (fun get ->
                { optionalField = get.Optional.Field "name" Decode.string }
            )

    let actual =
        Decode.fromString decoder json

"module ``getOptionalField returns Error if type is incorrect``" |> logtitle
``getOptionalField returns Error if type is incorrect``.actual |> log

HR()

module ``getDOTRequiredDOTAt works`` =
    open Thoth.Json

    // The only place we can use `at` here is the node `user`, 
    // since it is the parent of `name` and `age`
    let json = """{"user" : {"name":"maxime", "age":25}}""".Trim()
    let expected = Ok({fieldA = "maxime"})

    let decoder =
        Decode.object
            (fun get ->
                {fieldA = get.Required.At ["user"; "name";] Decode.string}
            )
    let actual = 
        Decode.fromString decoder json

"module ``getDOTRequiredDOTAt works``" |> logtitle
``getDOTRequiredDOTAt works``.actual |> log
``getDOTRequiredDOTAt works``.actual = ``getDOTRequiredDOTAt works``.expected |> log

HR()

module ``getDOTRequiredDOTAt returns Error if non object in path`` =
    open Thoth.Json
    let json = """{"user": "maxime"}""".Trim()

    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.At ["user"; "name"] Decode.string}
            )
    let actual =
        Decode.fromString decoder json

"module ``getDOTRequiredDOTAt returns Error if non object in path``" |> logtitle
``getDOTRequiredDOTAt returns Error if non object in path``.actual |> log

HR()

module ``getDOTRequiredDOTAt returns Error if field missing`` =
    open Thoth.Json
    let json = """{ "user": {"name":"maxime", "age":25}}""".Trim()

    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.At [ "user"; "firstname"] Decode.string }
            )

    let actual =
        Decode.fromString decoder json

"module ``getDOTRequiredDOTAt returns Error if field missing``" |> logtitle
``getDOTRequiredDOTAt returns Error if field missing``.actual |> log

HR()

module ``getDOTRequiredDOTAt returns Error if type is incorrect`` =
    open Thoth.Json
    let json = """{ "user": {"name":12, "age":25}}"""

    let decoder =
        Decode.object
            (fun get ->
                { fieldA = get.Required.At [ "user"; "name"] Decode.string}
            )
    let actual =
        Decode.fromString decoder json

"module ``getDOTRequiredDOTAt returns Error if type is incorrect``" |> logtitle
``getDOTRequiredDOTAt returns Error if type is incorrect``.actual |> log

HR()

module ``getDOTOptionalDOTAt works`` =
    open Thoth.Json
    let json = """{ "user": { "name": "maxime", "age": 25 } }"""
    let expected = Ok({optionalField = Some "maxime"})

    let decoder =
        Decode.object
            (fun get ->
                { optionalField = get.Optional.At ["user"; "name"] Decode.string}
            )

    let actual =
        Decode.fromString decoder json

"module ``getDOTOptionalDOTAt works``" |> logtitle
``getDOTOptionalDOTAt works``.actual |> log

HR()

module ``getDOTOptionalDOTAt returns type error if non object in path`` =
    open Thoth.Json
    let json = """{ "user": "maxime" }"""

    let decoder =
        Decode.object
            (fun get ->
                { optionalField = get.Optional.At [ "user"; "name" ] Decode.string }
            )
    let actual =
        Decode.fromString decoder json

"module ``getDOTOptionalDOTAt returns type error if non object in path``" |> logtitle
``getDOTOptionalDOTAt returns type error if non object in path``.actual |> log

HR()

module ``getDOTOptionalDOTAt returns None if field is missing`` =
    open Thoth.Json
    let json = """{ "user": { "name": "maxime", "age": 25 } }"""
    let expected = Ok({optionalField = None})

    let decoder =
        Decode.object
            (fun get ->
                { optionalField = get.Optional.At ["user"; "firstname"] Decode.string }
            )

    let actual =
        Decode.fromString decoder json

"module ``getDOTOptionalDOTAt returns None if field is missing``" |> logtitle
``getDOTOptionalDOTAt returns None if field is missing``.actual |> log

HR()

module ``getDOTOptionalDOTAt returns Error if type is incorrect`` =
    open Thoth.Json
    let json = """{ "user": { "name": 12, "age": 25 } }"""

    let decoder =
        Decode.object
            (fun get ->
                { optionalField = get.Optional.At ["user"; "name"] Decode.string }
            )
    
    let actual =
        Decode.fromString decoder json

"module ``getDOTOptionalDOTAt returns Error if type is incorrect``" |> logtitle
``getDOTOptionalDOTAt returns Error if type is incorrect``.actual |> log

HR()

module ``complex object builder works`` =
    open Thoth.Json
    let json = """{ "id": 67, "email": "user@mail.com" }"""
    let userDecoder =
        Decode.object
            (fun get ->
                {
                    Id = get.Required.Field "id" Decode.int
                    Name = get.Optional.Field "name" Decode.string |> Option.defaultValue ""
                    Email = get.Required.Field "email" Decode.string
                    Followers = 0
                }
            )

    let actual = 
        Decode.fromString userDecoder json

"module ``complex object builder works``" |> logtitle
``complex object builder works``.actual |> log

HR()

(**
https://github.com/MangelMaxime/Thoth/issues/51
Compose the object decoder with a custom decoder.
*)

module ``get Field Raw works`` =
    open Thoth.Json

    type Shape =
        | Circle of radius: int
        | Rectangle of width: int * height: int

        static member DecoderCircle =
            Decode.field "radius" Decode.int
            |> Decode.map Circle

        static member DecoderRectangle =
            Decode.tuple2
                (Decode.field "width" Decode.int)
                (Decode.field "height" Decode.int)
            |> Decode.map Rectangle

    type MyObj =
        { Enabled: bool; Shape: Shape }

    let json =
        """{
    "enabled": true,
    "shape": "circle",
    "radius": 20
        }""".Trim()

    // We first need to detect the shape without moving further in the path.
    // By using, `field` we can stay at the same position in the decoded object.
    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen 
            (function
            | "circle" -> 
                Shape.DecoderCircle
            | "rectangle" ->
                Shape.DecoderRectangle
            | shape ->
                Decode.fail (sprintf "Unknown shape type %s" shape)
            )
    
    // get.Required.Raw is basically a custom decoder.
    // It is like Decode.oneOf but here, if you have an invalid type, you get an error telling you the type isn't valid.
    // It basically allows you to compose the object decoder with a custom decoder.
    // This allows you to decode JSON without an obligatory field.
    let decoder = 
        Decode.object 
            (fun get ->
                { 
                    Enabled = get.Required.Field "enabled" Decode.bool
                    // Allow use of custom sub-decoder via get.Require.Raw decoder
                    Shape = get.Required.Raw shapeDecoder
                } : MyObj
            )

    let actual =
        Decode.fromString decoder json
    
    let expected =
        Ok ({Enabled = true; Shape = Circle 20} : MyObj)

"module ``get Field Raw works``" |> logtitle
``get Field Raw works``.actual |> log

HR()

module ``get Field Raw returns Error if a decoder fails`` =
    open Thoth.Json
    let json = 
        """{
"enabled": true,
"shape": "custom_shape",
"radius": 20
        }""".Trim()

    let shapeDecoder =
        Decode.field "shape" Decode.string
        // Decode.andThen does not really "decode", but just allows flow-continuation
        |> Decode.andThen 
            (function
                | "circle" -> 
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )

    let decoder =
        Decode.object
            (fun get ->
                { 
                    Enabled = get.Required.Field "enabled" Decode.bool
                    Shape = get.Required.Raw shapeDecoder
                } : MyObj
            )
    
    let actual =
        Decode.fromString decoder json

"module ``get Field Raw returns Error if a decoder fails``" |> logtitle
``get Field Raw returns Error if a decoder fails``.actual |> log

HR()

module ``get Field Raw returns Error if a field is missing in the raw decoder`` =
    open Thoth.Json
    let json = 
        """{
"enabled": true,
"shape": "circle",
"radiusTypo": 2000
        }""".Trim()

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen 
            (function
                | "circle" ->
                    Shape.DecoderCircle // raw decoder
                | "rectangle" ->
                    Shape.DecoderRectangle // raw decoder
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )

    let decoder =
        Decode.object 
            (fun get ->
                { 
                    Enabled = get.Required.Field "enabled" Decode.bool 
                    Shape = get.Required.Raw shapeDecoder
                } : MyObj
            )
    
    let actual =
        Decode.fromString decoder json

"module ``get Field Raw returns Error if a field is missing in the raw decoder``" |> logtitle
``get Field Raw returns Error if a field is missing in the raw decoder``.actual |> log

HR()

module ``getOptionalRaw works`` =
    open Thoth.Json

    let json = 
        """{
"enabled": true,
"shape": "circle",
"radius": 20
        }""".Trim()

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen 
            (function
                | "circle" ->
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )
    
    let decoder =
        Decode.object
            (fun get ->
                { 
                    Enabled = get.Required.Field "enabled" Decode.bool
                    // This optional refers to the raw JSON data
                    Shape = get.Optional.Raw shapeDecoder
                }
            )

    let actual =
        Decode.fromString decoder json
    
    let expected =
        Ok {
            Enabled = true
            Shape = Some (Circle 20)
        }

"module ``getOptionalRaw works``" |> log
``getOptionalRaw works``.actual |> log

HR()

module ``getOptionalRaw returns None if a field is missing`` =
    open Thoth.Json

    // missing field `radius` for custom raw-decoder Shape.DecoderCircle
    let json = 
        """{
            "enabled": true,
    	    "shape": "circle"
        }""".Trim()

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen
            (function
                | "circle" -> 
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )
    
    let decoder =
        Decode.object
            (fun get ->
                {
                    Enabled = get.Required.Field "enabled" Decode.bool
                    Shape = get.Optional.Raw shapeDecoder
                }
            )
    
    let actual = 
        Decode.fromString decoder json

    let expected =
        Ok { Enabled = true; Shape = None }

"``getOptionalRaw returns None if a field is missing``" |> logtitle
``getOptionalRaw returns None if a field is missing``.actual |> log

HR()

module ``getOptionalRaw returns an Error if a decoder fail`` =
    open Thoth.Json
    let json =
        """
        {
            "enabled": true,
            "shape": "invalid_shape"
        }
        """.Trim()

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen
            (function
                | "circle" ->
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )

    let decoder =
        Decode.object
            (fun get ->
                {
                    Enabled = get.Required.Field "enabled" Decode.bool
                    Shape = get.Optional.Raw shapeDecoder
                }
            )
    
    let actual =
        Decode.fromString decoder json

"module ``getOptionalRaw returns an Error if a decoder fail``" |> logtitle
``getOptionalRaw returns an Error if a decoder fail``.actual |> log

HR()

module ``getOptionalRaw returns an Error if the type is invalid`` =
    open Thoth.Json
    let json =
        """
        {
            "enabled": true,
            "shape": "circle",
            "radius": "maxime"
        }
        """

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen
            (function
                | "circle" ->
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )
    
    let decoder =
        Decode.object
            (fun get ->
                {
                    Enabled = get.Required.Field "enabled" Decode.bool
                    Shape = get.Optional.Raw shapeDecoder
                }
            )
    
    let actual =
        Decode.fromString decoder json

"module ``getOptionalRaw returns an Error if the type is invalid``" |> logtitle
``getOptionalRaw returns an Error if the type is invalid``.actual |> log

HR()

module ``getOptionalRaw returns None if a decoder fails with null`` =
    open Thoth.Json
    let json =
        """
        {
            "enabled": true,
            "shape": null
        }
        """

    let shapeDecoder =
        Decode.field "shape" Decode.string
        |> Decode.andThen
            (function
                | "circle" ->
                    Shape.DecoderCircle
                | "rectangle" ->
                    Shape.DecoderRectangle
                | shape ->
                    Decode.fail (sprintf "Unknown shape type %s" shape)
            )

    let decoder =
        Decode.object
            (fun get ->
                {
                    Enabled = get.Required.Field "enabled" Decode.bool
                    Shape = get.Optional.Raw shapeDecoder
                }
            )

    let actual =
        Decode.fromString decoder json

"module ``getOptionalRaw returns None if a decoder fails with null``" |> logtitle
``getOptionalRaw returns None if a decoder fails with null``.actual |> log

HR()

module ``Object builders returns all the Errors`` =
    open Thoth.Json
    let json = 
        """
        { 
            "age": 25, 
            "fieldC": "not_a_number", 
            "fieldD": { "sub_field": "not_a_boolean" } 
        }
        """

    let decode =
        Decode.object
            (fun get ->
                {
                    FieldA = get.Required.Field "missing_field_1" Decode.string
                    FieldB = get.Required.At ["missing_field_2"; "sub_field"] Decode.string
                    FieldC = get.Optional.Field "fieldC" Decode.int
                                |> Option.defaultValue -1
                    FieldD = get.Optional.At [ "fieldD"; "sub_field"] Decode.bool
                                |> Option.defaultValue false
                }
            )

    let actual = 
        Decode.fromString decode json

"module ``Object builders returns all the Errors``" |> logtitle
``Object builders returns all the Errors``.actual |> log

HR()

//-------------------------------
// Auto Encoding and Decoding
//-------------------------------

SECTION()

"Auto Encoding and Decoding" |> log

SECTION()

module ``AutoDecodefromString works`` =
    open Thoth.Json
    open System
    let now = DateTime.Now
    let value =
        { a = 5
          b = "bar"
          c = [false, 3; true, 5; false, 10]
          d = [|Some(Foo 14); None|]
          e = Map [("oh", { a = 2.; b = 2. }); ("ah", { a = -1.5; b = 0. })]
          f = now
          g = set [{ a = 2.; b = 2. }; { a = -1.5; b = 0. }]
          h = TimeSpan.FromSeconds(5.)
        }
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<Record9>(json)

"module ``AutoDecodefromString works``" |> logtitle
``AutoDecodefromString works``.json |> log
``AutoDecodefromString works``.actual |> log

HR()

module ``Auto serialization works with recursive types`` =
    open Thoth.Json
    let len xs =
        let rec lenInner acc = function
            | Cons(_,rest) -> lenInner (acc + 1) rest
            | Nil -> acc
        lenInner 0 xs
    let li = Cons(1, Cons(2, Cons(3, Nil)))
    let json = Encode.Auto.toString(4, li)
    let actual = Decode.Auto.unsafeFromString<MyList<int>>(json)

"module ``Auto serialization works with recursive types``" |> logtitle
``Auto serialization works with recursive types``.actual |> log

HR()

module ``Auto decoders work for string`` =
    open Thoth.Json
    let value = "maxime"
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<string> json

"module ``Auto decoders work for string``" |> logtitle
``Auto decoders work for string``.actual |> log

HR()

module ``Auto decoders work for guid`` =
    open Thoth.Json
    open System
    let value = Guid.NewGuid()
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<Guid>(json)

"module ``Auto decoders work for guid``" |> logtitle
``Auto decoders work for guid``.json |> log
``Auto decoders work for guid``.actual |> log

HR()

module ``Audo decoders work for int`` =
    open Thoth.Json
    let value = 12
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<int>(json)

"module ``Audo decoders work for int``" |> logtitle
``Audo decoders work for int``.json |> log
``Audo decoders work for int``.actual |> log

HR()

module ``Auto decoders work for int64`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withInt64
    let value = 9999999999L
    let json = Encode.Auto.toString(4, value, extra=extra)
    let actual = Decode.Auto.unsafeFromString<int64>(json, extra=extra)

"module ``Auto decoders work for int64``" |> logtitle
``Auto decoders work for int64``.json |> log
``Auto decoders work for int64``.actual |> log

HR()

module ``Auto decoders work for uint32`` =
    open Thoth.Json
    let value = 12u
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<uint32>(json)

"module ``Auto decoders work for uint32``" |> logtitle
``Auto decoders work for uint32``.json |> log
``Auto decoders work for uint32``.actual |> log

HR()

module ``Auto decoders with for uint64`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withUInt64
    let value = 9999999999999999999UL
    let json = Encode.Auto.toString(4, value, extra=extra)
    let actual = Decode.Auto.unsafeFromString<uint64>(json, extra=extra)

"module ``Auto decoders with for uint64``" |> logtitle
``Auto decoders with for uint64``.json |> log
``Auto decoders with for uint64``.actual |> log

HR()

module ``Auto decoders works for bigint`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withBigInt
    let value = 99999999999999999999999I
    let json = Encode.Auto.toString(4, value, extra=extra)
    let actual = Decode.Auto.unsafeFromString<bigint>(json, extra=extra)

"module ``Auto decoders works for bigint``" |> logtitle
``Auto decoders works for bigint``.json |> log
``Auto decoders works for bigint``.actual |> log

HR()

module ``Auto decoders works for bool`` =
    open Thoth.Json
    let value = false
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<bool>(json)

"module ``Auto decoders works for bool``" |> logtitle
``Auto decoders works for bool``.json |> log
``Auto decoders works for bool``.actual |> log

HR()

module ``Auto decoders works for float`` =
    open Thoth.Json
    let value = 12.
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<float>(json)

"module ``Auto decoders works for float``" |> logtitle
``Auto decoders works for float``.json |> log
``Auto decoders works for float``.actual |> log

HR()

module ``Auto decoders works for decimal`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withDecimal
    let value = 0.7833M
    let json = Encode.Auto.toString(4, value, extra=extra)
    let actual = Decode.Auto.unsafeFromString<decimal>(json, extra=extra)

"module ``Auto decoders works for decimal``" |> logtitle
``Auto decoders works for decimal``.json |> log
``Auto decoders works for decimal``.actual |> log

HR()

// This test is commented out in the source code -- why?
module ``Auto decoders works for datetime`` =
    open Thoth.Json
    open System
    let value = DateTime.Now
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<DateTime>(json)

"module ``Auto decoders works for datetime``" |> logtitle
``Auto decoders works for datetime``.json |> log
``Auto decoders works for datetime``.actual |> log

HR()

module ``Auto decoders works for datetime UTC`` =
    open Thoth.Json
    open System
    let value = DateTime.UtcNow
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<DateTime>(json)

"module ``Auto decoders works for datetime UTC``" |> logtitle
``Auto decoders works for datetime UTC``.json |> log
``Auto decoders works for datetime UTC``.actual |> log

HR()

module ``Auto decoders work for datetimeOffset`` =
    open Thoth.Json
    open System
    let value = DateTimeOffset.Now
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<DateTime>(json).ToLocalTime()

"module ``Auto decoders work for datetimeOffset``" |> logtitle
``Auto decoders work for datetimeOffset``.json |> log
``Auto decoders work for datetimeOffset``.actual |> log

``Auto decoders work for datetimeOffset``.actual.Date |> log
``Auto decoders work for datetimeOffset``.actual.Year |> log
``Auto decoders work for datetimeOffset``.actual.Month |> log
``Auto decoders work for datetimeOffset``.actual.Day|> log
``Auto decoders work for datetimeOffset``.actual.Hour |> log
``Auto decoders work for datetimeOffset``.actual.Minute |> log
``Auto decoders work for datetimeOffset``.actual.Second |> log
``Auto decoders work for datetimeOffset``.actual.Millisecond |> log

HR()

module ``Auto decoders works for datetimeOffset UTC`` =
    open Thoth.Json
    open System
    let value = DateTimeOffset.UtcNow
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<DateTimeOffset>(json).ToUniversalTime()
    
"module ``Auto decoders works for datetimeOffset UTC``" |> logtitle
``Auto decoders works for datetimeOffset UTC``.json |> log
``Auto decoders works for datetimeOffset UTC``.actual |> log

HR()

module ``Auto decoders works for TimeSpan`` =
    open Thoth.Json
    open System
    let value = TimeSpan(1, 2, 3, 4, 5)
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<TimeSpan>(json)

"module ``Auto decoders works for TimeSpan``" |> logtitle
``Auto decoders works for TimeSpan``.json |> log
``Auto decoders works for TimeSpan``.actual |> log
``Auto decoders works for TimeSpan``.actual.Days |> log
``Auto decoders works for TimeSpan``.actual.Hours |> log
``Auto decoders works for TimeSpan``.actual.Minutes |> log
``Auto decoders works for TimeSpan``.actual.Seconds |> log
``Auto decoders works for TimeSpan``.actual.Milliseconds |> log

HR()

module ``Auto decoders works for list`` =
    open Thoth.Json
    let value = [1; 2; 3; 4]
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<int list>(json)

"module ``Auto decoders works for list``" |> logtitle
``Auto decoders works for list``.json |> log
``Auto decoders works for list``.actual |> log

HR()

module ``Auto decoders works for array`` =
    open Thoth.Json
    let value = [| 1; 2; 3; 4; |]
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<int array>(json)

"module ``Auto decoders works for array``" |> logtitle
``Auto decoders works for array``.json |> log
``Auto decoders works for array``.actual |> log

HR()

module ``Auto decoders works for option None`` =
    open Thoth.Json
    let value = None
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<int option>(json)

"module ``Auto decoders works for option None``" |> logtitle
``Auto decoders works for option None``.json |> log
``Auto decoders works for option None``.actual |> log

HR()

module ``Auto decoders works for option Some`` =
    open Thoth.Json
    let value = Some "Middle Name"
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<string option>(json)

"module ``Auto decoders works for option Some``" |> logtitle
``Auto decoders works for option Some``.json |> log
``Auto decoders works for option Some``.actual |> log

HR()

module ``Auto decoders works for null`` =
    open Thoth.Json
    let value = null
    let json = Encode.Auto.toString(4, value)
    let actual = Decode.Auto.unsafeFromString<obj>(json)

"module ``Auto decoders works for null``" |> logtitle
``Auto decoders works for null``.json |> log
``Auto decoders works for null``.actual |> log

HR()

// This is not really that useful -- should always try to assign types
module ``Auto decoders works even if type is determined by the compiler`` =
    open Thoth.Json
    open System
    let value = ["1", "George", null, 1232.4]
    let json = Encode.Auto.toString(4, value)
    //let actual = Decode.Auto.unsafeFromString<_>(json) // not working anymore
    Decode.Auto.unsafeFromString<_>(json) |> log

"module ``Auto decoders works even if type is determined by the compiler``" |> logtitle
``Auto decoders works even if type is determined by the compiler``.json |> log

HR()

module ``Auto unsafe FromString works with camelCase`` =
    open Thoth.Json
    let json = """{ "id" : 0, "name": "maxime", "email": "mail@domain.com", "followers": 0 }"""
    let actual = Decode.Auto.unsafeFromString<User>(json, isCamelCase=true)
    
"``Auto unsafe FromString works with camelCase``" |> logtitle
``Auto unsafe FromString works with camelCase``.json |> log
``Auto unsafe FromString works with camelCase``.actual |> log

HR()

module ``Auto fromString works with camelCase`` =

    type User001 =
        { Id : int
          Name : string
          Email : string
          Followers : int }

    open Thoth.Json
    let json = """{ "id" : 0, "name": "maxime", "email": "mail@domain.com", "followers": 0 }"""
    let actual = Decode.Auto.fromString<User001>(json, isCamelCase=true)

"module ``Auto fromString works with camelCase``" |> logtitle
``Auto fromString works with camelCase``.json |> log
``Auto fromString works with camelCase``.actual |> log

HR()

module ``Auto fromString works for records with an actual value for the optional field value`` =
    open Thoth.Json
    let json = """{ "maybe" : "maybe value", "must": "must value"}"""
    let actual = Decode.Auto.fromString<TestMaybeRecord>(json, isCamelCase=true)

"module ``Auto fromString works for records with an actual value for the optional field value``" |> logtitle
``Auto fromString works for records with an actual value for the optional field value``.json |> log
``Auto fromString works for records with an actual value for the optional field value``.actual |> log

HR()

module ``Auto fromString works for records with null for the optional field value`` =
    open Thoth.Json
    let json = """{ "maybe" : null, "must": "must value"}"""
    let actual = Decode.Auto.fromString<TestMaybeRecord>(json, isCamelCase=true)

"module ``Auto fromString works for records with null for the optional field value``" |> logtitle
``Auto fromString works for records with null for the optional field value``.json |> logjson
``Auto fromString works for records with null for the optional field value``.actual |> log

HR()

module ``Auto fromString works for records missing optional field value on classes`` =
    open Thoth.Json
    let json = """{ "must": "must value" }"""
    let actual = Decode.Auto.fromString<RecordWithOptionalClass>(json, isCamelCase=true)

"module ``Auto fromString works for records missing optional field value on classes``" |> logtitle
``Auto fromString works for records missing optional field value on classes``.json |> logjson
``Auto fromString works for records missing optional field value on classes``.actual |> log

HR()

module ``Auto generateDecoder throws for field using a non optional class`` =
    open Thoth.Json

    let actual =
        try
            let decoder = Decode.Auto.generateDecoder<RecordWithRequiredClass>(isCamelCase=true) 
            ""
        with ex ->
            ex.Message

"module ``Auto generateDecoder throws for field using a non optional class``" |> logtitle
``Auto generateDecoder throws for field using a non optional class``.actual |> log

HR()

module ``Auto fromString works for Class marked as optional`` =
    open Thoth.Json
    let json = """null"""
    let actual = Decode.Auto.fromString<BaseClass option>(json, isCamelCase=true)

"module ``Auto fromString works for Class marked as optional``" |> logtitle
``Auto fromString works for Class marked as optional``.json |> logjson
``Auto fromString works for Class marked as optional``.actual |> log

HR()

module ``Auto generateDecoder throws for Class`` =
    open Thoth.Json
    let actual =
        try
            let decoder = Decode.Auto.generateDecoder<BaseClass>(isCamelCase=true)
            ""
        with ex ->
            ex.Message

"module ``Auto generateDecoder throws for Class``" |> logtitle
``Auto generateDecoder throws for Class``.actual |> log

HR()

module ``Auto fromString works for records missing an optional field`` =
    open Thoth.Json
    let json = """{ "must": "must value"}"""
    let actual = Decode.Auto.fromString<TestMaybeRecord>(json, isCamelCase=true)

"module ``Auto fromString works for records missing an optional field``" |> logtitle
``Auto fromString works for records missing an optional field``.json |> logjson
``Auto fromString works for records missing an optional field``.actual |> log

HR()

module ``Auto fromString works with maps encoded as objects`` =
    open Thoth.Json
    let json = """{"ah":{"a":-1.5,"b":0},"oh":{"a":2,"b":2}}"""
    let actual = Decode.Auto.fromString<obj>(json)

"module ``Auto fromString works with maps encoded as objects``" |> logtitle
``Auto fromString works with maps encoded as objects``.json |> logjson
``Auto fromString works with maps encoded as objects``.actual |> log

HR()

module ``Auto fromString works with maps encoded as arrays`` =
    open Thoth.Json
    let json = """[[{"a":-1.5,"b":0},"ah"],[{"a":2,"b":2},"oh"]]"""
    let actual = Decode.Auto.fromString<obj>(json)

"module ``Auto fromString works with maps encoded as arrays``" |> logtitle
``Auto fromString works with maps encoded as arrays``.json |> logjson
``Auto fromString works with maps encoded as arrays``.actual |> log

HR()

module ``Decoder Auto toString works with bigint extra`` =
    open Thoth.Json

    type BigIntRecord = { bigintField: bigint }

    let extra = Extra.empty |> Extra.withBigInt
    // Note: passing a record with bigint, not just a bigint 
    let value = { bigintField = 9999999999999999999999I }
    let json = Encode.Auto.toString(0, value, extra=extra)
    let actual = Decode.Auto.fromString<BigIntRecord>(json, extra=extra)
    
"module ``Decoder Auto toString works with bigint extra``" |> logtitle
``Decoder Auto toString works with bigint extra``.json |> logjson
``Decoder Auto toString works with bigint extra``.actual |> log

HR()

module ``Decoder Auto toString works with custom extra and raw Json`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withCustom ChildType.Encode ChildType.Decoder
    let json = """{"ParentField":"bumbabon"}"""
    let actual = Decode.Auto.fromString<ParentRecord>(json, extra=extra)

"module ``Decoder Auto toString works with custom extra and raw Json``" |> logtitle
``Decoder Auto toString works with custom extra and raw Json``.json |> logjson
``Decoder Auto toString works with custom extra and raw Json``.actual |> log

HR()

module ``Decoder Auto toString works with custom extra and constructed Json`` =
    open Thoth.Json
    let extra = Extra.empty |> Extra.withCustom ChildType.Encode ChildType.Decoder
    let value = { ParentField = { ChildField = "bumbabon" } }
    let json = Encode.Auto.toString(0, value, extra=extra)
    let actual = Decode.Auto.fromString<ParentRecord>(json, extra=extra)

"module ``Decoder Auto toString works with custom extra and constructed Json``" |> logtitle
``Decoder Auto toString works with custom extra and constructed Json``.json |> logjson
``Decoder Auto toString works with custom extra and constructed Json``.actual |> log

HR()

type RecordWithPrivateConstructor = private { Foo1: int; Foo2: float }

module ``Auto fromString works with records with private constructors`` =
    open Thoth.Json
    let json = """{ "foo1": 5, "foo2": 7.8 }"""
    let actual = Decode.Auto.fromString<RecordWithPrivateConstructor>(json, isCamelCase=true)

"module ``Auto fromString works with records with private constructors``" |> logtitle
``Auto fromString works with records with private constructors``.json |> logjson
``Auto fromString works with records with private constructors``.actual |> log

HR()

type UnionWithPrivateConstructor = private Bar of string | Baz

module ``Auto fromString works with unions with private construcors`` =
    open Thoth.Json
    let json = """[ "Baz", ["Bar", "foo"]]"""
    let actual = Decode.Auto.fromString<UnionWithPrivateConstructor list>(json, isCamelCase=true)

"``Auto fromString works with unions with private construcors``" |> logtitle
``Auto fromString works with unions with private construcors``.json |> logjson
``Auto fromString works with unions with private construcors``.actual |> log

HR()

module ``Auto generateDecoderCached works`` =
    open Thoth.Json
    let json = """{ "id" : 0, "name": "maxime", "email": "mail@domain.com", "followers": 0 }"""
    let decoder1 = Decode.Auto.generateDecoderCached<User>(isCamelCase=true)
    let decoder2 = Decode.Auto.generateDecoderCached<User>(isCamelCase=true)
    let actual1 = Decode.fromString decoder1 json
    let actual2 = Decode.fromString decoder2 json

"module ``Auto generateDecoderCached works``" |> logtitle
``Auto generateDecoderCached works``.json |> logjson
``Auto generateDecoderCached works``.actual1 |> log
``Auto generateDecoderCached works``.actual2 |> log

HR()

module ``Auto fromString works with strange types if they are None`` =
    open Thoth.Json
    let json = """{ "Id":0 }"""
    let actual = Decode.Auto.fromString<RecordWithStrangeType>(json)

"module ``Auto fromString works with strange types if they are None``" |> logtitle
``Auto fromString works with strange types if they are None``.json |> logjson
``Auto fromString works with strange types if they are None``.actual |> log

//-------------------------------
// Misc examples
//-------------------------------

SECTION()

"Misc examples" |> log

SECTION()

(**
Example of get.Required.At and Fetch
alfonsogarciacaro/fable-repl.css
https://gist.github.com/alfonsogarciacaro/f15cd60eef96da4f9cd5924fef21ecc7
https://randomuser.me/api/

*)
module ``ThothRandomUser example`` =
    open System
    open Thoth.Json

    (**
    We use get.Required.At to access Json in this parent-child structure:
    ```
    "name": {
        "title": "miss",
        "first": "lilou",
        "last": "roux"
    }
    ```
    To access and decode name.first, we use this syntax:
        `get.Required.At ["name"; "first"] Decode.string`
    *)

    type Gender =
        | Male
        | Female

        static member Decoder =
            Decode.string
            |> Decode.andThen(
                function
                | "male" -> Decode.succeed Male
                | "female" -> Decode.succeed Female
                | invalid -> "`" + invalid + "` isn't a valid value for Gender" |> Decode.fail
            )
    
    type User =
        {
            Gender : Gender
            FullName : string
            Email : string
            CellPhone : string
            OfficePhone : string
            Age : int
            Birthday : DateTime
            Picture : string
        }

        static member Decoder =
            Decode.object (fun get ->
                let firstname = get.Required.At ["name"; "first"] Decode.string
                let lastname = get.Required.At ["name"; "last"] Decode.string

                { 
                    Gender = get.Required.Field "gender" Gender.Decoder
                    FullName = firstname + " " + lastname
                    Email = get.Required.Field "email" Decode.string
                    CellPhone = get.Required.Field "cell" Decode.string
                    OfficePhone = get.Required.Field "phone" Decode.string
                    // Here `dob` is a JSON field returned from the server
                    Age = get.Required.At ["dob"; "age"] Decode.int
                    Birthday = get.Required.At ["dob"; "date"] Decode.datetime
                    Picture = get.Required.At ["picture"; "large"] Decode.string
                }
            )
    
    type UserList = 
        { userList : User list }
        static member Decoder =
            Decode.list User.Decoder

    let getRandomUser () = promise {
        let! response = Fetch.fetch "https://randomuser.me/api/" []
        let! responseText = response.text()

        "Raw JSON response:" |> logtitle
        responseText |> logjson

        let resultDecoder = 
            Decode.field "results" (Decode.index 0 User.Decoder)
        
        return Decode.fromString resultDecoder responseText
    }

"ThothRandomUser example" |> logtitle

``ThothRandomUser example``.getRandomUser() 
|> Promise.map (fun txt ->    
    txt
    |> sprintf "\nParsed Json response, accessed via Promise.Map:\n%A"
    |> log
    ) |> ignore
