% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/eyer-loading.R
\name{load_eyer_data}
\alias{load_eyer_data}
\title{Loads data from a folder. Loads preprocessed events, gaze and fixations files}
\usage{
load_eyer_data(folder, name = "", ...)
}
\arguments{
\item{folder}{what directory should be searcher? Not recursive}

\item{name}{name of the files to find in case there are multiple saved eyer objects in the folder}
}
\value{
EyerObject with filled in data or NULL if no files are found
}
\description{
eyer allows saving of preprocessed data with the function \code{eyer_save()}
this function then allows to load directly those prepared files into its structure. This function
returns NULL if there is no \link{name}_eyer_info.json file found in the directory.
}
