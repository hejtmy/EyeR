% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyer-preprocessing.R
\name{add_area_column}
\alias{add_area_column}
\title{Add area column to object data}
\usage{
add_area_column(obj, areas)
}
\arguments{
\item{obj}{\code{\link{EyerObject}}}

\item{areas}{list of \code{\link{AreaObject}}. Needs to be a list with area objects inside, otherwise it won't parse well}
}
\value{
modified \code{\link{EyerObject}} or the object back if something doesn't work
}
\description{
The newly added column has a stirng with the name of the area, or NA_character_
}
\details{
Adds new column to the fixations and gaze data frame from the area
}
