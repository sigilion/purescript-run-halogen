{ name = "run-halogen"
, dependencies =
  [ "aff"
  , "console"
  , "effect"
  , "free"
  , "halogen"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "run"
  , "tuples"
  , "typelevel-prelude"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
, license = "MIT"
, repository = "https://github.com/sigilion/purescript-run-halogen"
}
