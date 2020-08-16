{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "console"
  , "debug"
  , "effect"
  , "generics-rep"
  , "node-fs"
  , "ordered-collections"
  , "psci-support"
  , "xml"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
