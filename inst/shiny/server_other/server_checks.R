# Perform observe() checks of formatting of various vals components


###############################################################################
### Prep
sf.attr  <- c("names", "row.names", "class", "sf_column", "agr")
sfc.attr <- c(
  "n_empty", "crs", "class", "precision", "bbox", "classes", "names", "srid"
)

names.txt <- c("Pred", "SE", "Weight", "idx", "geometry")
names.df.txt <- c("Pred", "SE", "Weight", "idx")


modal.attr <- function(x) {
  modalDialog(
    title = x,
    paste("Please either contact Sam (sam.woodman@noaa.gov) or report this",
          "as an issue on GitHub (https://github.com/smwoodman/eSDM/issues)"),
    footer = tagList(modalButton("Close"))
  )
}


###############################################################################
### Ensure all original model (prediction) reactive values are correctly formatted
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
  if (!check.all) {
    showModal(modal.attr("Error: Improper formatting of original predictions (e1)"))
  }

   if (length(vals$models.ll) > 0) {
    check.all <- c(
      all(sapply(vals$models.ll, inherits, "sf")),
      all(sapply(vals$models.orig, inherits, "sf")),

      all(sapply(lapply(vals$models.ll, names), function(i) identical(i, names.txt))),
      all(sapply(lapply(vals$models.orig, names), function(i) identical(i, names.txt))),

      all(sapply(lapply(vals$models.ll,   function(i) names(st_agr(i))), function(j) identical(j, names.df.txt))),
      all(sapply(lapply(vals$models.orig, function(i) names(st_agr(i))), function(j) identical(j, names.df.txt))),

      all(sapply(vals$models.ll, function(i) identical(st_crs(i), st_crs(4326)))),

      all(sapply(vals$models.ll, st_agr) == "constant"),
      all(sapply(vals$models.orig, st_agr) == "constant"),

      all(sapply(vals$models.ll, attr, "sf_column") == "geometry"),
      all(sapply(vals$models.orig, attr, "sf_column") == "geometry"),

      sapply(lapply(vals$models.ll, function(i) i$Pred), is.numeric),
      sapply(lapply(vals$models.ll, function(i) i$SE), is.numeric),
      sapply(lapply(vals$models.ll, function(i) i$Weight), is.numeric),
      sapply(lapply(vals$models.ll, function(i) i$idx), is.numeric),

      all(sapply(vals$models.names, inherits, "character")),
      all(sapply(vals$models.data.names, function(i) all(sapply(i, inherits, "character"))))
    )

    if (!all(check.all) | anyNA(check.all)) {
      showModal(modal.attr("Error: Improper formatting of original predictions (e2)"))
    }
  }
})


###############################################################################
# Perform checks of various sf objects to make sure attributes are correct
# Checks are not performed on plot objects, i.e. pretty plot params list b/c
#   some preview360 code adds attributes b/c of st_intersection()

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
    all(sapply(vals$overlaid.models, inherits, "data.frame")),
    all(sapply(vals$overlaid.models, nrow) == nrow(vals$overlaid.models[[1]])),
    all(sapply(lapply(vals$overlaid.models, names), function(i) identical(i, names.df.txt))),
    all(vapply(vals$overlaid.specs, length, 1) == 10)
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
    all(sapply(vals$ensemble.models, inherits, "data.frame")),
    all(sapply(vals$ensemble.models, nrow) == nrow(vals$ensemble.models[[1]])),
    all(sapply(lapply(vals$ensemble.models, names), function(i) identical(i, c("Pred_ens", "SE_ens")))),
    all(vapply(vals$ensemble.specs, length, 1) == 6)
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
    showModal(modal.attr("Error in processing exclusion polygons"))
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
