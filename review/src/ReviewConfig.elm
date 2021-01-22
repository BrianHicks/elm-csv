module ReviewConfig exposing (config)

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import NoRedundantConcat
import NoRedundantCons
import NoUnused.CustomTypeConstructorArgs
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ -- elm-review-common
      NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoImportingEverything.rule [ "Test" ]
        |> Rule.ignoreErrorsForDirectories [ "tests/VerifyExamples" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule

    -- elm-review-unused
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.CustomTypeConstructorArgs.rule
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule

    -- elm-review-noredundantconcat
    , NoRedundantConcat.rule

    -- elm-review-noredundantcons
    , NoRedundantCons.rule
    ]
