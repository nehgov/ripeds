% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init.R
\name{ipeds_init}
\alias{ipeds_init}
\title{Initialize chained request.}
\usage{
ipeds_init(local_dir = NA, use_revised_files = TRUE, use_nse = TRUE)
}
\arguments{
\item{local_dir}{Local directory to check for complete IPEDS zip files that
have already been downloaded. Newly downloaded files will be placed here as
well. If unset, all files will be stored in tempdir().}

\item{use_revised_files}{When IPEDS zip files contain revised data, use the
revised files. Default is TRUE.}

\item{use_nse}{Use non-standard evaluation in chain. Default is TRUE.}
}
\description{
Initialize the data request. It should always be the first in the series of
piped functions.
}
\examples{
\dontrun{
ipeds_init()

# set local directory to search first for existing IPEDS data files and in
# which to save any that must be downloaded
ipeds_init(local_dir = ".")

# use original files even if revise files exist
ipeds_init(use_revised_files = FALSE)

# use standard evaluation throughout the ipeds_*() chain
ipeds_init(use_nse = FALSE)
}
}
