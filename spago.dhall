{ name =
    "run-halogen"
, dependencies =
    [ "effect", "console", "psci-support", "halogen", "run" ]
, packages =
    ./packages.dhall
, sources =
    [ "src/**/*.purs", "test/**/*.purs" ]
, license =
    "MIT"
, repository =
    "https://github.com/sigilion/purescript-run-halogen"
}
