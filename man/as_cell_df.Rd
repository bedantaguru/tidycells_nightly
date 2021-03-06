% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as_cell_df.R
\name{as_cell_df}
\alias{as_cell_df}
\title{Transform data into Cell-DF Structure}
\usage{
as_cell_df(d, take_col_names = TRUE, take_row_names = FALSE, ...)
}
\arguments{
\item{d}{the data (either a matrix with column name or a data.frame)}

\item{take_col_names}{consider column names as separate cells
(applicable only for data with no (row, col) information). Default is \code{TRUE}.}

\item{take_row_names}{consider row names as separate cells
(applicable only for data with no (row, col) information). Default is \code{FALSE}.}
}
\value{
An object of class \code{\link[=cell_df-class]{cell_df}}.

\strong{Note}: After this, you may like to do \code{\link[=value_attribute_classify]{Value Attribute Classification}}.
}
\description{
Transform an R object (mostly matrix or data.frame) into a \code{\link[=cell_df-class]{cell_df}}
for further processing in other \code{tidycells} functions.
}
\examples{

as_cell_df(iris)

# consider column name as cell
as_cell_df(iris, take_col_names = TRUE)

# if the data is already in a similar format it will not further transform
}
\seealso{
\itemize{
\item \code{\link[=validate_cells]{validate_cells}} which is used to validate \code{cell_df}.
\item \code{\link[unpivotr:as_cells]{as_cells}} from \code{unpivotr} package.
}
}
