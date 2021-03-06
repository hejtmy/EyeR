% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyer-preprocessing.R
\name{mark_eye_data}
\alias{mark_eye_data}
\title{Allows marking of eye data}
\usage{
mark_eye_data(obj, mark, times = c(), raw_times = FALSE)
}
\arguments{
\item{obj}{\code{\link{EyerObject}}}

\item{mark}{string with the name of the mark}

\item{times}{numeric(2) vector to define which part of the data to select}

\item{raw_times}{commonly, the eyer data times are 0 based witht eh starting time being saved in \code{info$start_time}.
Most operations are then calculated using these 0 based timings. If raw_times is set to TRUE,
data are filtered with ackowledging obj$info$start_time.}
}
\value{
Eyer object with preprocessed data fields
}
\description{
Allows marking of eye data
}
\details{
Adds a new column of mark- allows adding erroneous recordings etc.
}
