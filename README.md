# elm-csv

Decode CSV in the boringest way possible.
Other CSV libraries have exciting, innovative APIs.
Not this one!
Pretend you're writing a [JSON decoder](https://package.elm-lang.org/packages/elm/json/latest/), gimme your data, get on with your life.

```elm
import Csv.Decode as Decode exposing (Decoder)


type alias Pet =
    { id : Int
    , name : String
    , species : String
    }


decoder : Decoder Pet
decoder =
    Decode.map3 Pet
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.field "species" Decode.string)


csv : String
csv =
    "id,name,species\u{000D}\n1,Atlas,cat\u{000D}\n2,Pippi,dog"


Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv
--> Ok
-->     [ { id = 1, name = "Atlas", species = "cat" }
-->     , { id = 2, name = "Pippi", species = "dog" }
-->     ]
```

However, in an effort to avoid a common problem with `elm/json` ("how do I decode records with more than 8 fields?") this library also exposes a [pipeline-style decoder](https://package.elm-lang.org/packages/NoRedInk/elm-json-decode-pipeline/latest/) for anything above 3 fields:

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
    "id,name,species,weight\u{000D}\n1,Atlas,cat,14.5\u{000D}\n2,Pippi,dog,"


Decode.decodeCsv Decode.FieldNamesFromFirstRow decoder csv
--> Ok
-->     [ { id = 1, name = "Atlas", species = "cat", weight = Just 14.5 }
-->     , { id = 2, name = "Pippi", species = "dog", weight = Nothing }
-->     ]
```

## Contributing

This probject uses [Nix](https://nixos.org/download.html) to manage versions (but just need a `nix` installation, not NixOS, so this will work on macOS.)
Install that, then run `nix-shell` to get into a development environment.
(Or set up `direnv` and then `direnv allow`.
Whatever!)

Things I'd appreciate seeing PRs for:

- Adding decoders for things you find necessary when using this library (but please open an issue first so we can talk through it!)
  Examples: `json : Json.Decode.Decoder a -> Decoder a` or `result : (String -> Result String a) -> Decoder a`.
  I haven't written enough code using this library yet to get a sense for what's a common requirement!
- Benchmarking and performance improvements.
  Internally, this just uses `List` for everything.
  Some smart application of `Array` could potentially perform a lot better, but I have held off optimizing since I haven't measured!
- Docs, always docs.
  Forever docs.

## Climate Action

I want my open-source activities to support projects addressing the climate crisis (for example, projects in clean energy, public transit, reforestation, or sustainable agriculture.)
If you are working on such a project, and find a bug or missing feature in any of my libraries, **please let me know and I will treat your issue as high priority.**
I'd also be happy to support such projects in other ways.
In particular, I've worked with Elm for a long time and would be happy to advise on your implementation.

## License

`elm-csv` is licensed under the BSD 3-Clause license, located at `LICENSE`.
