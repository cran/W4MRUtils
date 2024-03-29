% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logging.R
\name{W4MLogger_.message__}
\alias{W4MLogger_.message__}
\title{W4MLogger_.message__}
\arguments{
\item{level}{is a string. By default its value should be either "info",
"debug", "warning", "debug", "verbose" or "INTERNAL".
But, if the logger was build with a different color naming, one of
the names provided in the "coloring" \code{named list} parameter must be
used, as it determines the color to use.}

\item{...}{anything, of any length. If this is not a character vector,
then, its displayable value will be obtained with
\code{capture.output(str(...))}
If the resulting character vector's length is greater than one,
then multiple messages will be printed.}
}
\value{
this logger's instance ( \code{.self} )
}
\description{
The function W4MLogger$.message__ is the function that gets
automatically called when  W4MLogger$info, W4MLogger$debug,
W4MLogger$warning, W4MLogger$error or W4MLogger$verbose are invoked.
This function is not truly internal, so it has to be considered
as external, but should not be exported:

This means its has to do type checking of its inputs, and consider
parameters as unsafe.

See \link{get_logger} for example usages.
}
