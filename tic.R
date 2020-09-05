# Copied over from sf/.tic.R, and then pared down as possible
do_package_checks()

if (ci_on_travis() && ci_has_env("BUILD_PKGDOWN")) {
  # creates pkgdown site and pushes to gh-pages branch
  # only for the runner with the "BUILD_PKGDOWN" env var set
  do_pkgdown()
}
