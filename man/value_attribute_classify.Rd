% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/value_attribute_classify.R
\name{value_attribute_classify}
\alias{value_attribute_classify}
\title{Value/Attribute Classifier}
\usage{
value_attribute_classify(d, classifier = basic_classifier())
}
\arguments{
\item{d}{a Cell DF}

\item{classifier}{a classifier}
}
\value{
a Cell DF with Value/Attribute Classification. The underlying tibble will contain an extra column named \code{type}.
}
\description{
After \code{\link[=as_cell_df]{as_cell_df}} (entry  point to \code{tidycells}) you may need to use this function
or individual \emph{Value/Attribute Classifier}-functions as listed below in \emph{"see also"} - section.

Here the idea is to classify all cells into either \code{value}, \code{attribute}, \code{empty} which will be
used by \code{\link[=analyze_cells]{analyze_cells}} for further processing.
}
\details{
In order to understand the data orientation and detect data-blocks Cell DF requires additional column named \code{type}.
This \code{type} column potentially contains either \code{value}, \code{attribute}, \code{empty}. The \code{value} are given corresponding to cells
with observations in it. The tag, \code{attribute} is for the identifier of these cells. Lastly, \code{empty} cells are useless cells or
cells with no meaningful information.

For \code{classifier} following options are present:
\itemize{
\item \code{basic_classifier} : naive classifier which recode \code{data_type}.
\item \code{sample_based_classifier} : sample-based classifier.
\item \code{numeric_values_classifier} : considers number like cells as values.
}

Each of the above are available as individual functions. Those can also be directly applied on a \code{cell-df}.
However, it is recommended to use \code{value_attribute_classify} as it tests for integrity after classification.
}
\examples{

iris \%>\%
  as_cell_df() \%>\%
  sample_based_classifier(value_sample = "setosa") \%>\%
  plot()

iris \%>\%
  as_cell_df() \%>\%
  sample_based_classifier(value_sample = "setosa") \%>\%
  numeric_values_classifier() \%>\%
  plot()

if (rlang::is_installed("tidyxl")) {
  cdn <- system.file("extdata", "RBI_HBS_Table_No_166.xlsx", package = "tidycells") \%>\%
    tidyxl::xlsx_cells()
  cdn <- cdn \%>\%
    dplyr::filter(sheet == sheet[1]) \%>\%
    as_cell_df()

  # all of these are same except value_attribute_classify will perform validate_cells once again
  cd1 <- sample_based_classifier(cdn, value_sample = "APR")
  cd2 <- sample_based_classifier(value_sample = "APR")(cdn)
  cd3 <- value_attribute_classify(cdn,
    classifier = sample_based_classifier(value_sample = "APR")
  )
  # see it
  plot(cd3)
}
}
\seealso{
Individual classifier functions:
\itemize{
\item \link[=basic_classifier]{basic_classifier}
\item \link[=sample_based_classifier]{sample_based_classifier}
\item \link[=numeric_values_classifier]{numeric_values_classifier},
}

For interactive Value/Attribute Classification check \link[=visual_va_classify]{visual_va_classify}
}
