###############################################################################
# Get dimensions for eSDM preview within the app
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
    x == 1 ~ 1.0,
    TRUE ~ 1.3
  )
  main.cex.curr <- case_when(
    x == 1 ~ 1,
    TRUE ~ 1.2
  )

  leg.lcm <- 3.0
  leg.txt.cex <- ifelse(x == 1, 0.8, 1.3)

  c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm, leg.txt.cex)
}


###############################################################################
# Get dimensions for eSDM preview being downloaded
multiplot_download <- function(x) {
  plot.ncol <- case_when(
    x <= 2 ~ 1,
    x <= 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )
  plot.nrow <- case_when(
    x == 1 ~ 1,
    x <= 4 ~ 2,
    x <= 9 ~ 3,
    TRUE ~ ceiling(sqrt(x))
  )

  axis.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x <= 4 ~ 0.6,
    TRUE ~ 0.6
  )
  main.cex.curr <- case_when(
    x == 1 ~ 0.8,
    x == 2 ~ 0.6,
    TRUE ~ 0.4
  )

  leg.lcm <- 2.9
  leg.txt.cex <- 0.7

  c(plot.ncol, plot.nrow, axis.cex.curr, main.cex.curr, leg.lcm, leg.txt.cex)
}

###############################################################################
