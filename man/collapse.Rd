% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{collapse}
\alias{collapse}
\title{collapse - to paste strings with collapse = ""}
\usage{
collapse(..., sep = "")
}
\arguments{
\item{...}{passed to \code{\link[base:paste]{base::paste0()}}}

\item{sep}{set the separator. Deafult is ""}
}
\description{
collapse does exactly when paste does, but default collapse = ""
}
\examples{

collapse(list("a message ", "in multiple", "parts"))

}
\author{
L.Pavot
}
