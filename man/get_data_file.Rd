% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data_file.R
\name{get_data_file}
\alias{get_data_file}
\title{Get Data File (from Dataverse)}
\usage{
get_data_file(file_name, outdir = "")
}
\arguments{
\item{file_name}{name of file to get from the dataverse dataset.}

\item{outdir}{target directory if downloading the file.}
}
\description{
Leaving outdir blank will provide the data directly.
If outdir is not blank, the data file will be saved to the (already created) directory specified.
}
\examples{
\dontrun{get_data_file("file_name.csv.xz", "./outdir")}
}
