module ThothExamples
open Tests.Types
(**
### From Tests to Working Code
* Read the official tests and understand them
* Since the tests should be exhaustive, work-through / identify the range of possible cases
* Adapt useful examples into usable code-snippets
* Base your code upon the tests, referencing the naming, etc. 
*)

// open Fable.Core.JsInterop // !^

(**
http://fsharp.org/specs/language-spec/3.0/FSharpSpec-3.0-final.pdf
Any sequence of characters that is enclosed in double-backtick marks (``   ``), excluding newlines,tabs , and double-back tick pairs themselves, is treated as an identifier. Note that when an identifier is used for the name of a type, union type case, module, or namespace, the following characters are not allowed even inside double-backtick marks:

‘.', '+', '$', '&', '[', ']', '/', '\\', '*', '\"', '`'
*)


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
    let expected = Ok("steffen")
    let actual =
        Decode.fromString (Decode.index 2 Decode.string) json

``index works``.actual |> log
``index works``.actual = ``index works``.expected |> log
match ``index works``.actual with
| Ok values -> values |> log
| Error err -> failwith err |> log

SECTION()

"Data structure: List" |> log

SECTION()

module ``list works`` =
    open Thoth.Json
    let expected = Ok([1; 2; 3])
    let actual =
        Decode.fromString (Decode.list Decode.int) "[1, 2, 3]"

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

``array works``.actual |> log
``array works``.actual = ``array works``.expected |> log
``array works``.actual 
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log


SECTION()

"Data structure: keyValue Pairs" |> log

SECTION()

module ``keyValuePairs works`` =
    open Thoth.Json
    let expected = Ok([("a", 1) ; ("b", 2) ; ("c", 3)])
    let actual =
        Decode.fromString (Decode.keyValuePairs Decode.int) """{ "a":1, "b":2, "c":3}"""


``keyValuePairs works``.actual |> log
``keyValuePairs works``.actual = ``keyValuePairs works``.expected |> log
``keyValuePairs works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log


SECTION()

"Data structure: dict" |> log

SECTION()


module ``dict works`` =
    open Thoth.Json
    let expected : Result<Map<string,int>,string> = Ok(Map.ofList([("a",1); ("b", 2); ("c", 3)]))
    let actual = 
        Decode.fromString (Decode.dict Decode.int) """{ "a": 1, "b": 2, "c": 3 }"""

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


``dict with custom decoder works``.actual |> log
``dict with custom decoder works``.actual  = ``dict with custom decoder works``.expected |> log

``dict with custom decoder works``.actual
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log


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

``oneOf works in combination with object builders``.json |> log
``oneOf works in combination with object builders``.actual |> log
//``oneOf works in combination with object builders``.expected = ``oneOf works in combination with object builders``.expected |> log

``oneOf works in combination with object builders``.expected
|> function
| Ok values -> values |> log
| Error err -> failwith err |> log


(**
### Choice type:

https://fsharpforfunandprofit.com/posts/railway-oriented-programming-carbonated/

The definition of the Choice type is as follows

 type Choice<'a, 'b> =
      | Choice1Of2 of 'a
      | Choice2Of2 of 'b


Good question! In the original post (see http://fsharpforfunandprofi... ) and talk (http://fsharpforfunandprofi... ) I DO use a special discriminated union.

Some people suggested that I should use a Choice so as to be compatible with other F# code (such as async -- https://msdn.microsoft.com/... ) so I thought that in this post I'd show how to be compatible with Choice using active patterns.



https://msdn.microsoft.com/en-us/visualfsharpdocs/conceptual/core.choice%5B't1%2C't2%5D-union-%5Bfsharp%5D
Core.Choice<'T1,'T2> Union (F#)
Helper types for active patterns with two choices.

Syntax
[<StructuralEquality>]
[<StructuralComparison>]
type Choice<'T1,'T2> =
| Choice1Of2 of 'T1
| Choice2Of2 of 'T2
with
interface IStructuralEquatable
interface IComparable
interface IComparable
interface IStructuralComparable
end

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


``oneOf works with optional``.actual1 |> log
``oneOf works with optional``.actual1 = ``oneOf works with optional``.expected1 |> log

``oneOf works with optional``.actual2 |> log
``oneOf works with optional``.actual2 = ``oneOf works with optional``.expected2 |> log

``oneOf works with optional``.actual3 |> log
``oneOf works with optional``.actual3 = ``oneOf works with optional``.expected3 |> log

``oneOf works with optional``.actual4 |> log
``oneOf works with optional``.actual4 = ``oneOf works with optional``.expected4 |> log


HR()

// line 890
module ``oneOf output errors if all case fails`` =
    open Thoth.Json
    let badInt =
        Decode.oneOf [ Decode.string; Decode.field "test" Decode.string;  ]
    let actual =
        Decode.fromString (Decode.list badInt) "[1,2,null,4]"

``oneOf output errors if all case fails``.actual |> log


HR()


module ``optional works`` =
    open Thoth.Json
    let json = """{ "name": "maxime", "age": 25, "something_undefined": null }"""

    let expectedValid = Ok(Some "maxime")
    let actualValid =
        Decode.fromString (Decode.optional "name" Decode.string) json

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

//------------

// stop at testCase "optionalAt works" line 951







