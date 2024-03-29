% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qgis-help.R
\name{qgis_show_help}
\alias{qgis_show_help}
\alias{qgis_get_description}
\alias{qgis_get_argument_specs}
\alias{qgis_get_output_specs}
\title{Get detailed information about one algorithm}
\usage{
qgis_show_help(algorithm)

qgis_get_description(algorithm)

qgis_get_argument_specs(algorithm, ...)

qgis_get_output_specs(algorithm, ...)
}
\arguments{
\item{algorithm}{A qualified algorithm name
(e.g., \code{"native:buffer"}).}

\item{...}{For internal use only.}
}
\value{
\itemize{
\item \code{qgis_get_description()}: a string.
\item \code{qgis_get_argument_specs()}, \code{qgis_get_output_specs()}: a tibble.
\item \code{qgis_show_help()}: the algorithm name, invisibly.
}
}
\description{
Get detailed information about one algorithm
}
\examples{
\dontshow{if (has_qgis()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
qgis_get_description("native:filedownloader")
\donttest{
# not running below examples in R CMD check to save time
qgis_get_argument_specs("native:filedownloader")
qgis_get_output_specs("native:filedownloader")
qgis_show_help("native:filedownloader")
}
\dontshow{\}) # examplesIf}
}
\seealso{
Other topics about information on algorithms & processing providers: 
\code{\link{qgis_algorithms}()},
\code{\link{qgis_search_algorithms}()}
}
\concept{topics about information on algorithms & processing providers}
