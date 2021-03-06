% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_functions_from_files.R
\name{getFunctionsFromFiles}
\alias{getFunctionsFromFiles}
\title{Get functions from a file}
\usage{
getFunctionsFromFiles(paths, output_env = NULL, libraries = NULL)
}
\arguments{
\item{paths}{a vector of paths to parse. if you pass in a directory, it will
walk that directory, looking for any files that end in R or Rmd}

\item{output_env}{An environment that you want the results written to.   ( TODO Deprecate)}

\item{libraries}{an environment, where keys are the names of libraries.}
}
\description{
In a future version:
TODO: should output a list of missing pacakges
}
\examples{
file_path = system.file( "test-files/simple_test.R", package = 'remembr')
env = getFunctionsFromFiles(file_path)
convertCallCountsToHashTable(env$cards)


}
