load("@io_tweag_rules_haskell//haskell:haskell.bzl",
  "haskell_toolchain",
  "haskell_doctest_toolchain",
  "haskell_binary",
  "haskell_import",
)

exports_files([
    "hazel.bzl",
    "BUILD.ghc",
    "paths-template.hs",
    "cc_configure_custom.bzl",
])

haskell_doctest_toolchain(
    name = "doctest",
    doctest = "@haskell_doctest//:doctest_bin",
)

haskell_import(name = "base")
haskell_import(name = "base16-bytestring")
haskell_import(name = "cryptohash")
haskell_import(name = "http-types")
haskell_import(name = "lens-aeson")
haskell_import(name = "http-client")
haskell_import(name = "http-client-tls")
haskell_import(name = "filepath")
haskell_import(name = "aeson")
haskell_import(name = "bytestring")
haskell_import(name = "lens")
haskell_import(name = "text")
haskell_import(name = "containers")
haskell_import(name = "temporary")
haskell_import(name = "process")
haskell_import(name = "pretty")
haskell_import(name = "Cabal")
haskell_import(name = "yaml")

haskell_binary(
    name = "stackage_hs",
    srcs =
        [ "Stackage.hs",
          "//hazel_base_repository:Skylark.hs"
        ],
    deps =
        [ "base",
          "base16-bytestring",
          "cryptohash",
          "http-types",
          "http-client",
          "http-client-tls",
          "lens-aeson",
          "lens",
          "aeson",
          "filepath",
          "bytestring",
          "text",
          "containers",
          "temporary",
          "process",
          "pretty",
          "Cabal",
          "yaml",
        ]
        )

genrule(
    name = "foo_bar",
    outs = ["packages.bzl"],
    srcs = [ "@stackage_lts//file", "@all_cabal_hashes_repo//:all" ],
    cmd = "$(location stackage_hs) $(location @stackage_lts//file) external/all_cabal_hashes_repo/commercialhaskell-all-cabal-hashes-1a28e3e  $@",
    tools = ["//:stackage_hs"],
)
