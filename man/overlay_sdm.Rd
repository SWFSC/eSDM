% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/overlay_sdm.R
\name{overlay_sdm}
\alias{overlay_sdm}
\title{Overlay SDM predictions onto base geometry}
\usage{
overlay_sdm(base.geom, sdm, sdm.idx, overlap.perc)
}
\arguments{
\item{base.geom}{object of class \code{sfc}; base geometry}

\item{sdm}{object of class \code{sf}; original SDM predictions}

\item{sdm.idx}{names or indices of column(s) with data to be overlaid}

\item{overlap.perc}{numeric; percent overlap threshold,
i.e. percentage of each base geometry polygon must overlap with SDM
prediction polygons for overlaid density value to be calculated and
not set as NA}
}
\value{
Object of class \code{sf} with the geometry of \code{base.geom} and
  the data in the \code{sdm.idx} columns of \code{sdm} overlaid onto that
  geometry. Note that this means all columns of \code{sdm} not in
  \code{sdm.idx} will not be in the returned object.
  Because the data are considered spatially intensive, the \code{agr}
  attribute will be set as 'constant' for all columns in the returned object.

  Additionally, the output will match the class of \code{sdm}, with regards
  to the classes tbl_df, tbl, and data.frame. This means that, in addition to
  being an \code{sf} object, if \code{sdm} is a tibble then the output will
  also be a tibble, while if \code{sdm} is just a data frame then the output
  will not be a tibble.
}
\description{
Overlay specified SDM predictions that meet the percent overlap threshold requirement onto base geometry
}
\details{
See the eSDM GUI manual for specifics about the overlay process.
  This process is equivalent to areal interpolation (Goodchild and Lam 1980),
  where \code{base.geom} is the target, \code{sdm} is the source, and the data
  specified by \code{sdm.idx} are spatially intensive.

  Note that \code{overlay_sdm} removes rows in \code{sdm} that have NA values
  in the first column specified in \code{sdm.idx} (i.e. \code{sdm.idx[1]}),
  before the overlay.
  Thus, for valid overlay results, all columns of \code{sdm} specified in
  \code{sdm.idx} must either have NA values in the same rows
  or contain only NAs.
}
\examples{
pol1.geom <- sf::st_sfc(
  sf::st_polygon(list(rbind(c(1,1), c(3,1), c(3,3), c(1,3), c(1,1)))),
  crs = sf::st_crs(4326)
)
pol2.geom <- sf::st_sfc(
  sf::st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0)))),
  crs = sf::st_crs(4326)
)
pol2.sf <- sf::st_sf(data.frame(Dens = 0.5), geometry = pol2.geom,
                     crs = sf::st_crs(4326))

overlay_sdm(pol1.geom, pol2.sf, 1, 25)

# Output 'Dens' value is NA because of higher overlap.perc value
overlay_sdm(pol1.geom, pol2.sf, 1, 50)

\dontshow{if (FALSE) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
# These examples take longer to run
overlay_sdm(sf::st_geometry(preds.1), preds.2, 1, 50)
overlay_sdm(sf::st_geometry(preds.2), preds.1, "Density", 50)
\dontshow{\}) # examplesIf}
}
\references{
Goodchild, M.F. & Lam, N.S.-N. (1980) Areal interpolation:
  a variant of the traditional spatial problem. Geo-Processing, 1, 297-312.
}
