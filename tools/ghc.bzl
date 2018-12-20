default_ghc_workspaces = {
  "linux": "@ghc",
  "mac os x": "@ghc",
  "windows": "@ghc-windows",
}

def get_ghc_workspace(ghc_workspaces, os):
  """Return the GHC workspace appropriate for the given OS."""
  if os.name not in ghc_workspaces:
    fail("No known GHC workspace for the OS {} in {}".format(
      os.name, ghc_workspaces))
  return ghc_workspaces[os.name]


def get_ghc_executable(ghc_workspace, os):
  """Return the GHC executable in the given workspace for the given OS."""
  if os.name == "windows":
    ghc = "{}//:bin/ghc.exe".format(ghc_workspace)
  else:
    ghc = "{}//:bin/ghc".format(ghc_workspace)
  return ghc
