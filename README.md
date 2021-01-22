# elm-csv

Decode CSV in the most boring way possible.

Other CSV libraries have exciting, innovative APIs... not this one!
Pretend you're writing a [JSON decoder](https://package.elm-lang.org/packages/elm/json/latest/), gimme your data, get on with your life.

```elm
import Csv.Decode as Decode exposing (Decoder)


decoder : Decoder ( Int, Int, Int )
decoder =
    Decode.map3 (\r g b -> ( r, g, b ))
        (Decode.column 0 Decode.int)
        (Decode.column 1 Decode.int)
        (Decode.column 2 Decode.int)


csv : String
csv =
    "0,128,128\r\n112,128,144"


Decode.decodeCsv Decode.NoFieldNames decoder csv
--> Ok
-->     [ ( 0, 128, 128 )
-->     , ( 112, 128, 144 )
-->     ]
```

However, in an effort to avoid a common problem with `elm/json` ("how do I decode records with more than 8 fields?") this library also exposes a [pipeline-style decoder](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/) for records:

```elm
import Csv.Decode as Decode exposing (Decoder)


type alias Pet =
    { id : Int
    , name : String
    , species : String
    , weight : Maybe Float
    }


decoder : Decoder Pet
decoder =
    Decode.pipeline Pet
        |> Decode.required (Decode.field "id" Decode.int)
        |> Decode.required (Decode.field "name" Decode.string)
        |> Decode.required (Decode.field "species" Decode.string)
        |> Decode.required (Decode.field "weight" (Decode.blank Decode.float))


csv : String
csv =
    "id,name,species,weight\r\n1,Atlas,cat,14.5\r\n2,Pippi,dog,"


Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv
--> Ok
-->     [ { id = 1, name = "Atlas", species = "cat", weight = Just 14.5 }
-->     , { id = 2, name = "Pippi", species = "dog", weight = Nothing }
-->     ]
```

## FAQ

### Can this do TSVs too?

Yep!
Use `decodeCustom`.
It takes a field and row separator string, which can be whatever you need.

### Aren't there like (*checks*) 8 other CSV libraries already?

Yes, there are!
And while I appreciate the hard work that other people have put into those, there are a couple problems:

First, you need to put together multiple libraries to successfully parse CSV.
Usually you'll use something like [`lovasoa/elm-csv`](https://package.elm-lang.org/packages/lovasoa/elm-csv/latest/) to parse into a `List (List String)`, and then something like [`ericgj/elm-csv-decode`](https://package.elm-lang.org/packages/ericgj/elm-csv-decode/latest/) to convert from a grid of strings into the values you care about.

Props to those authors for making their hard work available, of course, but this situation bugs me!
I don't want to have to pick different libraries for parsing and converting.
I just want it to work like `elm/json` where I write a decoder, give the package a string, and handle a `Result`.
This should not require so much thought!

The second thing, and the one that prompted me to finally do something about this, is that none of the libraries available implement `andThen`.
Sure, you can use a `Result` to do whatever you like, but there's not a good way to combine make decoding decisions dependent on the fields you see.

## Contributing

This probject uses [Nix](https://nixos.org/download.html) to manage versions (but just need a `nix` installation, not NixOS, so this will work on macOS.)
Install that, then run `nix-shell` to get into a development environment.

Things I'd appreciate help with:

- Testing the parser on many kinds of CSV and TSV data.
  If you find that some software produces something that this library can't handle, please open an issue with a sample!

- Feedback on speed.
  For the data sizes I'm working with in my use of this library, speed is unlikely to be an issue.
  If you're parsing a *lot* of data, thought, it may be for you.
  If you find that this library has become a bottleneck in your application, please open an issue.

- Feedback on decoders for things you find necessary (but please open an issue and talk through it instead of jumping straight to a PR!)
  Some things I've thought of: `parse : Parser.Parser a -> Decoder a`, `json : Json.Decode.Decoder a -> Decoder a`, `liftResult : (String -> Result String a) -> Decoder a`.
  If you have concrete cases for any of those, let's talk about it!

Things I'd appreciate seeing PRs for, which we probably don't need to coordinate much on other than a heads-up that you're doing the work:

- Benchmarking and performance improvements.
  Internally, we just use `List` for everything.
  Some smart application of `Array` could potentially perform a lot better, but I have held off optimizing since I haven't measured!

- Docs.
  Always docs.
  Forever docs.

## Climate Action

I want my open-source activities to support projects addressing the climate crisis (for example, projects in clean energy, public transit, reforestation, or sustainable agriculture.)
If you are working on such a project, and find a bug or missing feature in any of my libraries, **please let me know and I will treat your issue as high priority.**
I'd also be happy to support such projects in other ways.
In particular, I've worked with Elm for a long time and would be happy to advise on your implementation.

## License

`elm-csv` is licensed under the BSD 3-Clause license, located at `LICENSE`.
