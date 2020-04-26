# # installs dependencies, runs R CMD check, runs covr::codecov()
# do_package_checks()
#
# if (ci_on_travis() && ci_has_env("BUILD_PKGDOWN")) {
#   # creates pkgdown site and pushes to gh-pages branch
#   # only for the runner with the "BUILD_PKGDOWN" env var set
#   do_pkgdown()
# }

do_package_checks()

get_stage("install") %>%
  # install lwgeom with its own library since linking again postgis source install fails sometimes
  add_code_step(install.packages("lwgeom", configure.args="--without-liblwgeom"))

###
# deploy pkgdowm site
###
do_pkgdown(document = FALSE)
