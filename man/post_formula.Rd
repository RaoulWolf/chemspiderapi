% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FILTERING-post_formula.R
\name{post_formula}
\alias{post_formula}
\title{POST a chemical formula to obtain a query ID}
\usage{
post_formula(
  formula,
  data_sources = NULL,
  order_by = "recordId",
  order_direction = "ascending",
  apikey = NULL
)
}
\arguments{
\item{formula}{A character string of a chemical formula.}

\item{data_sources}{Optional: Either a single character string or a vector of character string specifying the data sources. A list of possible data sources can be obtained from \code{chemspiderapi::get_datasources()}.}

\item{order_by}{A character string indicating by which parameter the results should be ordered; see Details.}

\item{order_direction}{A character string indicating in which direction the results should be ordered; see Details.}

\item{apikey}{A 32-character string with a valid key for ChemSpider's API services.}
}
\value{
Returns the queryId string as (named) character vector.
}
\description{
Functionality to POST a formula to obtain a \code{query_id} for use in \code{chemspiderapi::get_query_id_status()} and \code{chemspiderapi::get_queryId_results()}.
}
\details{
Possible values for \code{order_by} are: \code{"recordId"} (default), \code{"massDefect"}, \code{"molecularWeight"}, \code{"referenceCount"}, \code{"dataSourceCount"}, \code{"pubmedCount"}, and \code{"rscCount"}.\cr
\cr
Possible values for \code{order_direction} are: \code{"ascending"} (default) and \code{"descending"}.\cr
\cr
Says ChemSpider:\cr
\cr
\emph{"If dataSources is not specified, all known sources are searched. This will take longer."}\cr
\cr
If successful, returns the \code{query_id} as character string.
}
\examples{
\dontrun{
## POST the formula of caffeine to get a query ID
formula <- "C8H10N4O2"
apikey <- "a valid 32-character ChemSpider apikey"
post_formula(formula = formula, apikey = apikey)
}
}
\seealso{
\url{https://developer.rsc.org/compounds-v1/apis/post/filter/formula}
}
\author{
Raoul Wolf (\url{https://github.com/RaoulWolf/})
}
