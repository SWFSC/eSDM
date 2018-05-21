###############################################################################
#
multiplot_inapp <- function(x) {
  plot.ncol <- case_when(
    x == 1 ~ 1,
    x == 2 ~ 2,
    x == 3 ~ 3,
    x == 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )
  plot.nrow <- case_when(
    x <= 3 ~ 1,
    x <= 6 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    # x == 1 ~ 1.2,
    x <= 3 ~ 1.2,
    TRUE ~ 0.6
  )
  main.cex.curr <- case_when(
    x == 1 ~ 1.6,
    TRUE ~ 1.0
  )

  leg.lcm <- case_when(
    TRUE ~ 3.0
  )

  return(c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm))
}


###############################################################################
#
multiplot_download <- function(x) {
  plot.ncol <- case_when(
    x == 1 ~ 1,
    x <= 6 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )
  plot.nrow <- case_when(
    x <= 2 ~ 1,
    x <= 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x <= 4 ~ 0.3,
    TRUE ~ 0.2
  )
  main.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x == 2 ~ 0.5,
    TRUE ~ 0.4
  )

  # x.tick.num <- 5
  # y.tick.num <- 5

  leg.lcm <- case_when(
    # x == 1 ~ 3.0,
    # x == 2 ~ 2.0,
    TRUE ~ 3.0
  )

  return(c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm))
}

###############################################################################
