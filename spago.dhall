{ name = "remote-house"
, dependencies = [ "console", "effect", "psci-support" ]
, packages = ./packages.dhall
, sources = [ "frontend/src/**/*.purs", "frontend/test/**/*.purs" ]
}
