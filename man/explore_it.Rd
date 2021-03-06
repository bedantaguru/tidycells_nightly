% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore_it.R
\name{explore_it}
\alias{explore_it}
\title{Explore recursively the content of the file or folder}
\usage{
explore_it(file_name, max_depth = Inf)
}
\arguments{
\item{file_name}{local file or folder path}

\item{max_depth}{max recursion depth (default is \code{Inf})}
}
\value{
a object of exploration_findings_class
}
\description{
Explore recursively the content of the file or folder
}
\examples{
fold <- system.file("extdata", "messy", package = "tidycells", mustWork = TRUE)
explore_it(fold)
}
