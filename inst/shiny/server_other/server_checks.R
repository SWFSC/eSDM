# Perform observe() checks of formatting of various vals components


###############################################################################
### Ensure all original model reactive values are correctly formatted
observe({
  req(length(vals$models.ll) > 0)
  vals$models.orig
  vals$models.names
  vals$models.data.names
  vals$models.pred.type
  vals$models.specs

  check.all <- zero_range(
    sapply(list(vals$models.ll, vals$models.orig, vals$models.names,
                vals$models.data.names, vals$models.pred.type,
                vals$models.specs),
           length)
  )
  if (!check.all) shinyjs::alert(
    paste("eSDM error 1: Improper formatting of original predictions;",
          "please either contact Sam at sam.woodman@noaa.gov or report this as an issue on GitHub")
  )

  if (length(vals$models.ll) > 0) {
    check.all <- c(
      all(sapply(vals$models.ll, inherits, "sf")),
      all(sapply(vals$models.orig, inherits, "sf")),

      all(sapply(lapply(vals$models.ll, names), function(i) identical(i, c("Pred", "Weight", "Pixels", "geometry")))),
      all(sapply(lapply(vals$models.orig, names), function(i) identical(i, c("Pred", "Weight", "Pixels", "geometry")))),

      all(sapply(lapply(vals$models.ll, function(i) names(st_agr(i))), function(j) identical(j, c("Pred", "Weight", "Pixels")))),
      all(sapply(lapply(vals$models.orig, function(i) names(st_agr(i))), function(j) identical(j, c("Pred", "Weight", "Pixels")))),

      all(sapply(vals$models.ll, function(i) identical(st_crs(i), st_crs(4326)))),

      all(sapply(vals$models.ll, st_agr) == "constant"),
      all(sapply(vals$models.orig, st_agr) == "constant"),

      all(sapply(vals$models.ll, attr, "sf_column") == "geometry"),
      all(sapply(vals$models.orig, attr, "sf_column") == "geometry"),

      sapply(lapply(vals$models.ll, function(i) i$Pred), is.numeric),
      sapply(lapply(vals$models.ll, function(i) i$Weight), is.numeric),
      sapply(lapply(vals$models.ll, function(i) i$Pixels), is.numeric),

      all(sapply(vals$models.names, inherits, "character")),
      all(sapply(vals$models.data.names, function(i) all(sapply(i, inherits, "character"))))
    )

    if (!all(check.all) | anyNA(check.all)) {
      shinyjs::alert(
        paste("eSDM error 2: Improper formatting of orignal predictions;",
              "please either contact Sam at sam.woodman@noaa.gov or report this as an issue on GitHub")
      )
    }
  }
})


###############################################################################
# Perform checks of various sf objects to make sure attributes are correct
# Checks are not performed on plot objects, i.e. pretty plot params list b/c
#   some preview360 code adds attributes b/c of st_intersection()

#------------------------------------------------------------------------------
### Prep
sf.attr  <- c("names", "row.names", "class", "sf_column", "agr")
sfc.attr <- c("n_empty", "crs", "class", "precision", "bbox", "classes", "names")
# TODO: research 'classes' attribute

modal.attr <- function(x) {
  modalDialog(
    title = x,
    "Please report this as an issue",
    footer = tagList(modalButton("Close"))
  )
}

#------------------------------------------------------------------------------
### Original predictions
observe({
  req(length(vals$models.ll) > 0)
  vals$models.orig

  check.all <- c(
    all(sapply(vals$models.ll, function(i) all(names(attributes(i)) %in% sf.attr))),
    all(sapply(vals$models.ll, function(i) all(names(attributes(st_geometry(i))) %in% sfc.attr))),
    all(sapply(vals$models.orig, function(i) all(names(attributes(i)) %in% sf.attr))),
    all(sapply(vals$models.orig, function(i) all(names(attributes(st_geometry(i))) %in% sfc.attr)))
  )

  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing original predictions"))
  }
})


#------------------------------------------------------------------------------
### Overlaid predictions and associated spatial objects
observe({
  req(length(vals$overlaid.models) > 0)
  check.all <- c(
    all(sapply(vals$overlaid.models, function(i) all(names(attributes(i)) %in% sf.attr))),
    all(sapply(vals$overlaid.models, function(i) all(names(attributes(st_geometry(i))) %in% sfc.attr)))
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing overlaid predictions"))
  }
})
observe({
  req(vals$overlay.bound)
  check.all <- c(
    all(names(attributes(vals$overlay.bound)) %in% sfc.attr)
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing study area polygon"))
  }
})

observe({
  req(vals$overlay.land)
  check.all <- c(
    all(names(attributes(vals$overlay.land)) %in% sfc.attr)
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing erasing polygon"))
  }
})

observe({
  req(vals$base.sfc)
  check.all <- c(
    all(sapply(vals$base.sfc, function(i) all(names(attributes(i)) %in% sfc.attr)))
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in creation of base geometry"))
  }
})


#------------------------------------------------------------------------------
### Ensemble predictions
observe({
  req(length(vals$ensemble.models) > 0)
  check.all <- c(
    all(sapply(vals$ensemble.models, function(i) all(names(attributes(i)) %in% sf.attr))),
    all(sapply(vals$ensemble.models, function(i) all(names(attributes(st_geometry(i))) %in% sfc.attr)))
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing ensemble predictions"))
  }
})

observe({
  req(vals$ens.over.wpoly.sf)
  req(any(sapply(vals$ens.over.wpoly.sf, isTruthy)))
  check.all <- c(
    all(sapply(vals$ens.over.wpoly.sf, function(i) {
      if (isTruthy(i)) {
        all(sapply(i, function(j) all(names(attributes(j)) %in% sf.attr)))
      } else {
        TRUE
      }
    }))
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing weight polygons"))
  }
})


#------------------------------------------------------------------------------
### Validation data
observe({
  req(vals$eval.data)
  check.all <- c(
    all(names(attributes(vals$eval.data)) %in% sf.attr)
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing validation data"))
  }
})


#------------------------------------------------------------------------------
### Additional objects
observe({
  req(vals$pretty.addobj)
  check.all <- c(
    all(sapply(vals$pretty.addobj, function(i) {
      if (inherits(i$obj, "sf")) {
        all(names(attributes(i$obj)) %in% sf.attr) && all(names(attributes(st_geometry(i$obj))) %in% sfc.attr)
      } else {
        all(names(attributes(i$obj)) %in% sfc.attr)
      }
    }))
  )
  if (!all(check.all) | anyNA(check.all)) {
    showModal(modal.attr("Error in processing additional objects"))
  }
})


###############################################################################
