# elm-csv

Decode CSV in the boringest way possible.
Other CSV libraries have exciting, innovative APIs.
Not this one!
Pretend it's JSON, gimme your data, get on with your life.

If you've used Elm for any amount of time, you have probably used `elm/json` to decode some value.
Congratulations!
That means you already know how to use this package.

```elm
import Csv.Decode as Decode exposing (Decoder)


type alias Person =
    { id : Int
    , name : String
    , -- cats are people too, OKAY!?
      isACat : Bool
    }


{-| Some CSV you got from somewhere. A file, maybe? Or a HTTP call? Or
someone pasting it in a text box? Whatever.
-}
csv : String
csv =
    "id,name,species\r\n1,Brian,human\r\n2,Atlas,kitty cat"


decoder : Decoder Person
decoder =
    Decode.map3 Person -- pipeline syntax also available!
        (Decode.field "id" Decode.int)
        (Decode.field "name" Decode.string)
        (Decode.andThen
            (\species ->
                case species of
                    "kitty cat" ->
                        Decode.succeed True

                    "human" ->
                        Decode.succeed False

                    _ ->
                        Decode.fail ("I don't know if a " ++ species ++ " is a cat.")
            )
        )
```

## Contributing

This probject uses Nix to manage versions.
Install that, then run `nix-shell` to get into a development environment.

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
