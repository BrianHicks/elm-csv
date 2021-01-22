module ReviewConfig exposing (config)

import NoExposingEverything
import NoImportingEverything
import NoMissingTypeAnnotation
import NoMissingTypeAnnotationInLetIn
import NoMissingTypeExpose
import Review.Rule as Rule exposing (Rule)


config : List Rule
config =
    [ NoExposingEverything.rule
        |> Rule.ignoreErrorsForDirectories [ "tests" ]
    , NoImportingEverything.rule [ "Test" ]
        |> Rule.ignoreErrorsForDirectories [ "tests/VerifyExamples" ]
    , NoMissingTypeAnnotation.rule
    , NoMissingTypeAnnotationInLetIn.rule
    , NoMissingTypeExpose.rule
    ]
